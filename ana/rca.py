import numpy as np
from typing import Tuple
from itertools import combinations
from math import comb

# find knee point of a curve
# y: array of y values
# x: array of x values (optional)
# returns the x value and the index of the x value
def knee_pt(y: np.array, x: np.array = None) -> Tuple[float, int]:
    res_x = float('nan')
    idx_of_result = float('nan')
    if x is None:
        x = np.arange(0, len(y))
    cv_x = x
    cv_y = y
    if len(x) != len(y):
        raise ValueError("x and y must have the same length")
    if len(x) < 3:
        raise ValueError("x and y must have at least 3 elements")
    ind_list = np.argsort(cv_x, axis=0)
    cv_x = np.take_along_axis(cv_x, ind_list, axis=0)
    cv_y = np.take_along_axis(cv_y, ind_list, axis=0)
    sigma_xy = np.cumsum(np.multiply(cv_x, cv_y))
    sigma_x = np.cumsum(cv_x)
    sigma_y = np.cumsum(cv_y)
    sigma_xx = np.cumsum(np.multiply(cv_x, cv_x))
    n = np.arange(1, len(y) + 1)
    det = np.multiply(n, sigma_xx) - np.multiply(sigma_x, sigma_x)
    mfwd = (np.multiply(n, sigma_xy) - np.multiply(sigma_x, sigma_y)) / det
    bfwd = -(np.multiply(sigma_x, sigma_xy) - np.multiply(sigma_xx, sigma_y)) / det
    
    sigma_xy = np.cumsum(np.multiply(cv_x[::-1], cv_y[::-1]))
    sigma_x = np.cumsum(cv_x[::-1])
    sigma_y = np.cumsum(cv_y[::-1])
    sigma_xx = np.cumsum(np.multiply(cv_x[::-1], cv_x[::-1]))

    det = np.multiply(n, sigma_xx) - np.multiply(sigma_x, sigma_x)
    mbck = np.flipud((np.multiply(n, sigma_xy) - np.multiply(sigma_x, sigma_y)) / det)
    bbck = np.flipud((np.multiply(sigma_xx, sigma_y) - np.multiply(sigma_x, sigma_xy)) / det)
    error_curve = np.empty(len(y))
    error_curve[:] = np.nan

    for breakpt in range(1, len(y)):
        delsfwd = (np.multiply(mfwd[breakpt], cv_x[0:(breakpt + 1)]) + bfwd[breakpt]) - cv_y[0:(breakpt + 1)]
        delsbck = (np.multiply(mbck[breakpt], cv_x[breakpt:]) + bbck[breakpt]) - cv_y[breakpt:]
        error_curve[breakpt] = np.sum(np.abs(delsfwd)) + np.sum(np.abs(delsbck))
    idx_of_result = np.nanargmin(error_curve)
    res_x = cv_x[idx_of_result]
    return res_x, ind_list[idx_of_result]

class RCAModel:
    def fit(self, data: np.ndarray, nReg: int = None, nComp: int = 3, condRange: slice = None, subjRange: slice = None) -> None:
        _, self.components_, _, _ = RCAModel.rcaRun(data, nReg, nComp, condRange, subjRange)
        return self

    def transform(self, data: np.ndarray) -> np.ndarray:
        return RCAModel.rcaProject(data, self.components_)
    
    def fit_transform(self, data: np.ndarray, nReg: int = None, nComp: int = 3, condRange: slice = None, subjRange: slice = None) -> np.ndarray:
        Y, self.components_, _, _ = RCAModel.rcaRun(data, nReg, nComp, condRange, subjRange)
        return Y

    # compute spatial filters maximizing reliability across trials given precomputed auto- and cross-covariance matrices
    # Rxx: auto-covariance matrix of X
    # Ryy: auto-covariance matrix of Y
    # Rxy: cross-covariance matrix of X and Y
    # K: number of autocovariance dimensions to diagonalize (defaults to the number of eigenvalues explaining 60% of total power)
    # C: number of components to return
    # returns: Wsub: channel x component spatial filters maximizing reliability across trials
    #          A: channel x component forward model
    #          W: channel x component matrix of all RCA projections
    #          dGenSort: sorted eigenvalues of the generalized eigenvalue problem
    #          K: number of autocovariance dimensions to diagonalize
    @staticmethod
    def rcaTrain(Rxx: np.matrix, Ryy: np.matrix, Rxy: np.matrix, K: int = None, C: int = 3) -> Tuple[np.matrix, np.matrix, np.matrix, np.matrix, int]:
        if Rxx.shape != Rxy.shape:
            raise ValueError("Rxx and Rxy must have the same shape")
        eValues, eVectors = np.linalg.eig(Rxx+Ryy)
        sort_indexes = np.argsort(np.abs(eValues), axis=0)
        eValues = np.take_along_axis(eValues, sort_indexes, axis=0)
        eVectors = eVectors[:, sort_indexes]
        if K is None:
            _, indices = knee_pt(eValues, np.arange(0, len(eValues)))
            K = len(eValues) - indices - 1
            
        eValues_index = np.argsort(eValues, axis=0)
        eValues = np.take_along_axis(eValues, eValues_index, axis=0)
        eVectors = eVectors[:, eValues_index]

        eValues = eValues[-1-K:]
        Rw = np.matmul(eVectors[:, -1-K:], np.matmul(np.diag(1 / eValues), np.matmul(np.conjugate(np.transpose(eVectors[:, -1-K:])), Rxy + np.conjugate(np.transpose(Rxy)))))
        

        dGen, vGen = np.linalg.eig(Rw)
        dGen_index = np.argsort(np.abs(dGen), axis=0)
        dGenSort = np.abs(dGen[dGen_index])

        W = vGen[:, dGen_index[::-1]]
        Wsub = W[:, :C]
        Rpool = 0.5 * (Rxx + Ryy)
        A = np.matmul(np.matmul(Rpool, Wsub), np.linalg.inv(np.matmul(np.conjugate(np.transpose(Wsub)), np.matmul(Rpool, Wsub))))
        return Wsub, A, W, dGenSort, K
    
    # across-trial covariance matrices for input into rcaTrain()
    # data: nCond x nSubjects array of nSamples x nElectrodes x nTrials data volumes
    # condRange: range of conditions to include in training (defaults to all conditions)
    # subjRange: range of subjects to include in training (defaults to all subjects)
    # returns: sumXX: nCond x nSubjects x nElectrodes x nElectrodes array of summed autocovariance matrices for X
    #          sumYY: nCond x nSubjects x nElectrodes x nElectrodes array of summed autocovariance matrices for Y
    #          sumXY: nCond x nSubjects x nElectrodes x nElectrodes array of summed cross-covariance matrices for X and Y
    #          nPointsInXX: nCond x nSubjects x nElectrodes x nElectrodes array of number of points in each summed autocovariance matrix for X
    #          nPointsInYY: nCond x nSubjects x nElectrodes x nElectrodes array of number of points in each summed autocovariance matrix for Y
    #          nPointsInXY: nCond x nSubjects x nElectrodes x nElectrodes array of number of points in each summed cross-covariance matrix for X and Y
    @staticmethod
    def preComputeRcaCovariances(data: np.ndarray, condRange: slice = None, subjRange: slice = None) -> Tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
        if condRange is None:
            condRange = slice(0, data.shape[0])
        if subjRange is None:
            subjRange = slice(0, data.shape[1])
        data = data[condRange, subjRange]
        nCond = data.shape[0]
        nSubjects = data.shape[1]
        print("selected ", nCond, " conditions and ", nSubjects, " subjects for training")
        nElectrodes = data[0, 0].shape[1]
        sumXX = np.zeros((nCond, nSubjects, nElectrodes, nElectrodes), dtype=np.complex128)
        sumYY = np.zeros((nCond, nSubjects, nElectrodes, nElectrodes), dtype=np.complex128)
        sumXY = np.zeros((nCond, nSubjects, nElectrodes, nElectrodes), dtype=np.complex128)
        nPointsInXX = np.zeros((nCond, nSubjects, nElectrodes, nElectrodes), dtype=int)
        nPointsInYY = np.zeros((nCond, nSubjects, nElectrodes, nElectrodes), dtype=int)
        nPointsInXY = np.zeros((nCond, nSubjects, nElectrodes, nElectrodes), dtype=int)
        
        for cond in range(0, nCond):
            for subj in range(0, nSubjects):
                print("computing covariances for condition ", cond, " and subject ", subj)
                thisVolume = data[cond, subj]
                nSamples = thisVolume.shape[0]
                nElectrodes = thisVolume.shape[1]
                if nSamples < nElectrodes:
                    print("WARNING: number of samples is less than number of electrodes")
                nTrials = thisVolume.shape[2]
                pindx = np.ndarray((comb(nTrials, 2), 2), dtype=int)
                for k, e in enumerate(combinations(range(0, nTrials), 2)):
                    pindx[k, :] = e
                pindx = np.concatenate((pindx, pindx[:, ::-1]), axis=0)
                nPairs = pindx.shape[0]
                
                sxt = nSamples * nTrials
                thisVolume = np.transpose(thisVolume, (1, 0, 2))
                thisVolume = np.reshape(thisVolume, (nElectrodes, sxt))
                thisMu = np.nanmean(thisVolume, axis=1)
                thisVolume = thisVolume - np.repeat(thisMu[:, np.newaxis], sxt, axis=1)
                thisVolume = np.reshape(thisVolume, (nElectrodes, nSamples, nTrials))
                
                sxp = nSamples * nPairs
                concatX = thisVolume[:, :, pindx[:, 0]]
                concatY = thisVolume[:, :, pindx[:, 1]]
                concatX = np.reshape(concatX, (nElectrodes, sxp))
                concatY = np.reshape(concatY, (nElectrodes, sxp))
                
                notNanX = ~np.isnan(concatX)
                notNanY = ~np.isnan(concatY)

                nPointsInXX[cond, subj] = np.matmul(notNanX, np.transpose(notNanX), dtype=int)
                nPointsInYY[cond, subj] = np.matmul(notNanY, np.transpose(notNanY), dtype=int)
                nPointsInXY[cond, subj] = np.matmul(notNanX, np.transpose(notNanY), dtype=int)

                np.nan_to_num(concatX, copy=False)
                np.nan_to_num(concatY, copy=False)

                sumXX[cond, subj] = np.matmul(concatX, np.conjugate(np.transpose(concatX)))
                sumYY[cond, subj] = np.matmul(concatY, np.conjugate(np.transpose(concatY)))
                sumXY[cond, subj] = np.matmul(concatX, np.conjugate(np.transpose(concatY)))

        return sumXX, sumYY, sumXY, nPointsInXX, nPointsInYY, nPointsInXY

    # project data onto spatial filters maximizing reliability across trials
    # data: nCond x nSubjects array of nSamples x nElectrodes x nTrials data volumes
    # W: channel x component spatial filters maximizing reliability across trials
    # returns: Y: nCond x nSubjects array of nSamples x nComp x nTrials RCA projections
    @staticmethod
    def rcaProject(data: np.ndarray, W: np.matrix) -> np.ndarray:
        nCond = data.shape[0]
        nSubjects = data.shape[1]
        nComp = W.shape[1]
        Y = np.ndarray((nCond, nSubjects), dtype=np.ndarray)
        for c in range(0, nCond):
            for s in range(0, nSubjects):
                data3D = data[c, s]
                Y[c, s] = np.ndarray((data3D.shape[0], nComp, data3D.shape[2]), dtype=np.complex128)
                nSamples, nElectrodes, nTrials = data3D.shape
                if nSamples < nElectrodes:
                    print("WARNING: number of samples is less than number of electrodes")
                if nElectrodes != W.shape[0]:
                    raise ValueError("number of electrodes in data and W must be the same")

                for comp in range(0, nComp):
                    rep = np.repeat(np.transpose(np.conjugate(W[:, comp])), nTrials)
                    st = np.stack([rep for _ in range(0, nSamples)], axis=0)
                    rst = np.reshape(st, [nSamples, W.shape[0], nTrials])
                    temp = np.multiply(data3D, rst)
                    temp = np.nansum(temp, axis=1)
                    
                    Y[c, s][:, comp, :] = temp
                nanIdx = np.isnan(data3D[:, :nComp, :])
                Y[c, s][nanIdx] = np.nan
        return Y
    
    # run RCA on data to perform dimensionality reduction and compute spatial filters maximizing reliability across trials
    # data: nCond x nSubjects array of nSamples x nElectrodes x nTrials data volumes
    # nReg: regularization parameter controlling the number of bases to
    #   diagonalize pooled autocovariance (must be positive integer less than the
    #   number of channels). Defaults to the number of bases diagonalizing 60%
    #   of pooled covariance. For EEG, do not set this to be the number of
    #   electrodes. Typical values range from 5-15.
    # nComp: number of components to return
    # condRange: range of conditions to include in training (defaults to all conditions)
    # subjRange: range of subjects to include in training (defaults to all subjects)
    # returns: Y: nCond x nSubjects array of nSamples x nComp x nTrials RCA projections
    #          Wsub: channel x component spatial filters maximizing reliability across trials
    #          A: channel x component forward model
    #          dGenSort: sorted eigenvalues of the generalized eigenvalue problem
    @staticmethod
    def rcaRun(data: np.ndarray, nReg: int = None, nComp: int = 3, condRange: slice = None, subjRange: slice = None) -> Tuple[np.ndarray, np.matrix, np.matrix, np.matrix]:
        if condRange is None:
            condRange = slice(0, data.shape[0])
        if subjRange is None:
            subjRange = slice(0, data.shape[1])
        sumXX, sumYY, sumXY, nPointsInXX, nPointsInYY, nPointsInXY = RCAModel.preComputeRcaCovariances(data, condRange, subjRange)
        
        Rxx = np.sum(np.sum(sumXX, axis=1), axis=0) / np.sum(np.sum(nPointsInXX, axis=1), axis=0)
        Ryy = np.sum(np.sum(sumYY, axis=1), axis=0) / np.sum(np.sum(nPointsInYY, axis=1), axis=0)
        Rxy = np.sum(np.sum(sumXY, axis=1), axis=0) / np.sum(np.sum(nPointsInXY, axis=1), axis=0)
        Wsub, A, _, dGenSort, _ = RCAModel.rcaTrain(Rxx, Ryy, Rxy, nReg, nComp)
        Y = RCAModel.rcaProject(data, Wsub)
        return Y, Wsub, A, dGenSort
    
def RCA() -> RCAModel:
    return RCAModel()
