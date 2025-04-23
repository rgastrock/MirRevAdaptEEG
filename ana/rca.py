import queue
from queue import Queue
import numpy as np
from typing import Tuple
from typing_extensions import Self
from itertools import combinations
from math import comb
import matplotlib.pyplot as plt
import threading

def execute(q: Queue) -> None:
    while True:
        t = q.get()
        if t is None:
            q.put(None)
            print("no more job exiting", flush=True)
            break
        f = t[0]
        args: Tuple = t[1]
        f(*args)

def covarianceVolume2D(thisVolume: np.ndarray, 
                       pindx: np.ndarray,
                       nSamples: int, 
                       nElectrodes: int,
                       nTrials: int,
                       nPairs: int,
                       cond: int,
                       subj: int,
                       nPointsInXX: np.ndarray,
                       nPointsInYY: np.ndarray,
                       nPointsInXY: np.ndarray,
                       sumXX: np.ndarray,
                       sumYY: np.ndarray,
                       sumXY: np.ndarray,
                       task_queue: Queue,
                       done_queue: Queue,
                       n_done: int) -> None:
    sxt = nSamples * nTrials
    sxp = nSamples * nPairs
    thisVolume = np.transpose(thisVolume, (1, 0, 2))
    thisVolume = np.reshape(thisVolume, (nElectrodes, sxt))
    thisMu = np.nanmean(thisVolume, axis=1, dtype=np.float32)
    thisVolume = thisVolume - np.repeat(thisMu[:, np.newaxis], sxt, axis=1)
    thisVolume = np.reshape(thisVolume, (nElectrodes, nSamples, nTrials))
    
    concatX = np.reshape(thisVolume[:, :, pindx[0, :]], (nElectrodes, sxp))
    concatY = np.reshape(thisVolume[:, :, pindx[1, :]], (nElectrodes, sxp))
    notNanX = ~np.isnan(concatX)
    notNanY = ~np.isnan(concatY)
    np.nan_to_num(concatX, copy=False)
    np.nan_to_num(concatY, copy=False)

    task_queue.put((covarianceSumPoints, (concatX,
                                                    concatX,
                                                    notNanX,
                                                    notNanX,
                                                    cond,
                                                    subj,
                                                    nPointsInXX,
                                                    sumXX,
                                          task_queue,
                                          done_queue,
                                          n_done)))
    task_queue.put((covarianceSumPoints, (concatY,
                                                    concatY,
                                                    notNanY,
                                                    notNanY,
                                                    cond,
                                                    subj,
                                                    nPointsInYY,
                                                    sumYY,
                                          task_queue,
                                          done_queue,
                                          n_done)))
    task_queue.put((covarianceSumPoints, (concatX,
                                                    concatY,
                                                    notNanX,
                                                    notNanY,
                                                    cond,
                                                    subj,
                                                    nPointsInXY,
                                                    sumXY,
                                          task_queue,
                                          done_queue,
                                          n_done)))
    print("done covarianceVolume2D", cond, subj, flush=True)

def covarianceSumPoints(concat0: np.ndarray,
                        concat1: np.ndarray,
                        notNan0: np.ndarray,
                        notNan1: np.ndarray,
                        cond: int,
                        subj: int,
                        nPoints: np.ndarray,
                        sums: np.ndarray,
                        task_queue: Queue,
                        done_queue: Queue,
                        n_done: int) -> None:
    print("conputing covarianceSumPoints", cond, subj, flush=True)
    nPoints[cond, subj] = np.matmul(notNan0, np.transpose(notNan1), dtype=int)
    sums[cond, subj] = np.matmul(concat0, np.transpose(concat1), dtype=np.float32)
    n = done_queue.get()
    n = n + 1
    done_queue.put(n)

    if n == n_done:
        task_queue.put(None)
    print("done covarianceSumPoints", cond, subj, flush=True)

# find knee point of a curve
# y: array of y values
# x: array of x values (optional)
# returns the x value and the index of the x value
def knee_pt(y: np.ndarray, x: np.ndarray | None = None) -> Tuple[float, int]:
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
    def fit(self, 
            data: np.ndarray, 
            nReg: int | None = None, 
            nComp: int = 3, 
            condRange: slice | None = None, 
            subjRange: slice | None = None, 
            cov_process_num: int = 1) -> Self:
        _, self.components_, self.A_, self.dGenSort_, self.Rxx_, self.Ryy_, self.Rxy_, self.K_ = RCAModel.rcaRun(data, nReg, nComp, condRange, subjRange, cov_process_num)
        return self

    def transform(self, data: np.ndarray) -> np.ndarray:
        return RCAModel.rcaProject(data, self.components_)
    
    def fit_transform(self, 
                      data: np.ndarray, 
                      nReg: int | None = None, 
                      nComp: int = 3, 
                      condRange: slice | None = None, 
                      subjRange: slice | None = None,
                      cov_process_num: int = 1) -> np.ndarray:
        Y, self.components_, self.A_, self.dGenSort_, self.Rxx_, self.Ryy_, self.Rxy_, self.K_ = RCAModel.rcaRun(data, nReg, nComp, condRange, subjRange, cov_process_num)
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
    def rcaTrain(Rxx: np.matrix, Ryy: np.matrix, Rxy: np.matrix, K: int | None = None, C: int = 3) -> Tuple[np.matrix, np.matrix, np.matrix, np.matrix, int]:
        if Rxx.shape != Rxy.shape:
            raise ValueError("Rxx and Rxy must have the same shape")
        eValues, eVectors = np.linalg.eig(Rxx+Ryy)
        eValues = eValues.astype(np.float32)
        eVectors = eVectors.astype(np.float32)

        sort_indexes = np.argsort(eValues, axis=0)
        eValues = np.take_along_axis(eValues, sort_indexes, axis=0)
        eVectors = eVectors[:, sort_indexes]
        if K is None:
            _, indices = knee_pt(eValues, np.arange(0, len(eValues)))
            K = len(eValues) - indices - 1

        eValues = eValues[-1-K:]
        Rw = np.matmul(eVectors[:, -1-K:], np.matmul(np.diag(1 / eValues), np.matmul(np.transpose(eVectors[:, -1-K:]), Rxy + np.transpose(Rxy))))
        
        dGen, vGen = np.linalg.eig(Rw)
        dGen = dGen.astype(np.float32)
        vGen = vGen.astype(np.float32)
        
        dGen_index = np.argsort(np.abs(dGen), axis=0)
        dGenSort = np.asmatrix(np.abs(dGen[dGen_index]))

        W = np.asmatrix(vGen[:, dGen_index[::-1]])
        Wsub = np.asmatrix(W[:, :C])
        Rpool = 0.5 * (Rxx + Ryy)
        A = np.matmul(np.matmul(Rpool, Wsub), np.linalg.inv(np.matmul(np.transpose(Wsub), np.matmul(Rpool, Wsub))))
        return Wsub, A, W, dGenSort, K
    
    # same as preComputeRcaCovariances, but use thread
    @staticmethod
    def preComputeRcaCovariancesThread(data: np.ndarray, condRange: slice | None = None, subjRange: slice | None = None, cov_process_num: int = 2) -> Tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
        if condRange is None:
            condRange = slice(0, data.shape[0])
        if subjRange is None:
            subjRange = slice(0, data.shape[1])
        data = data[condRange, subjRange]
        nCond = data.shape[0]
        nSubjects = data.shape[1]
        print("selected ", nCond, " conditions and ", nSubjects, " subjects for training")
        nElectrodes = data[0, 0].shape[1]
        sumXX = np.zeros((nCond, nSubjects, nElectrodes, nElectrodes), dtype=np.float32, order="F")
        sumYY = np.zeros((nCond, nSubjects, nElectrodes, nElectrodes), dtype=np.float32, order="F")
        sumXY = np.zeros((nCond, nSubjects, nElectrodes, nElectrodes), dtype=np.float32, order="F")
        nPointsInXX = np.zeros((nCond, nSubjects, nElectrodes, nElectrodes), dtype=int, order="F")
        nPointsInYY = np.zeros((nCond, nSubjects, nElectrodes, nElectrodes), dtype=int, order="F")
        nPointsInXY = np.zeros((nCond, nSubjects, nElectrodes, nElectrodes), dtype=int, order="F")
        
        pindx_cache = {}
        n_processes = cov_process_num
        if n_processes < 1:
            n_processes = 2
        
        print("process number:", n_processes)
        n_done = nCond * nSubjects * 3
        done_queue = queue.Queue(1)
        done_queue.put(0)
        task_queue = queue.LifoQueue()
        
        threads = tuple(threading.Thread(target=execute, args=(task_queue,)) for _ in range(n_processes))
        for th in threads:
            th.start()
        
        for cond in range(0, nCond):
            for subj in range(0, nSubjects):
                print("computing covariances for condition ", cond, " and subject ", subj)
                thisVolume = data[cond, subj]
                nSamples = thisVolume.shape[0]
                nElectrodes = thisVolume.shape[1]
                if nSamples < nElectrodes:
                    print("WARNING: number of samples is less than number of electrodes")
                nTrials = thisVolume.shape[2]
                pindx = None
                if nTrials in pindx_cache:
                    pindx = pindx_cache[nTrials]
                else:
                    pindx = np.ndarray((comb(nTrials, 2), 2), dtype=int)
                    for k, e in enumerate(combinations(range(0, nTrials), 2)):
                        pindx[k, :] = e
                    pindx = np.transpose(np.concatenate((pindx, pindx[:, ::-1]), axis=0))
                    pindx_cache[nTrials] = pindx

                nPairs = pindx.shape[1]
                task_queue.put((covarianceVolume2D, (thisVolume,
                                                          pindx,
                                                          nSamples,
                                                          nElectrodes,
                                                          nTrials,
                                                     nPairs,
                                                          cond,
                                                          subj,
                                                     nPointsInXX,
                                                          nPointsInYY,
                                                          nPointsInXY,
                                                          sumXX,
                                                          sumYY,
                                                          sumXY,
                                                          task_queue,
                                                          done_queue,
                                                          n_done)))
        for th in threads:
            th.join()
        print("done joining")
        return sumXX, sumYY, sumXY, nPointsInXX, nPointsInYY, nPointsInXY

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
    def preComputeRcaCovariances(data: np.ndarray, condRange: slice | None = None, subjRange: slice | None = None) -> Tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
        if condRange is None:
            condRange = slice(0, data.shape[0])
        if subjRange is None:
            subjRange = slice(0, data.shape[1])
        data = data[condRange, subjRange]
        nCond = data.shape[0]
        nSubjects = data.shape[1]
        print("selected ", nCond, " conditions and ", nSubjects, " subjects for training")
        nElectrodes = data[0, 0].shape[1]
        sumXX = np.zeros((nCond, nSubjects, nElectrodes, nElectrodes), dtype=np.float32, order="F")
        sumYY = np.zeros((nCond, nSubjects, nElectrodes, nElectrodes), dtype=np.float32, order="F")
        sumXY = np.zeros((nCond, nSubjects, nElectrodes, nElectrodes), dtype=np.float32, order="F")
        nPointsInXX = np.zeros((nCond, nSubjects, nElectrodes, nElectrodes), dtype=int, order="F")
        nPointsInYY = np.zeros((nCond, nSubjects, nElectrodes, nElectrodes), dtype=int, order="F")
        nPointsInXY = np.zeros((nCond, nSubjects, nElectrodes, nElectrodes), dtype=int, order="F")
        
        pindx_cache = {}

        for cond in range(0, nCond):
            for subj in range(0, nSubjects):
                print("computing covariances for condition ", cond, " and subject ", subj)
                thisVolume = data[cond, subj]
                nSamples = thisVolume.shape[0]
                nElectrodes = thisVolume.shape[1]
                if nSamples < nElectrodes:
                    print("WARNING: number of samples is less than number of electrodes")
                nTrials = thisVolume.shape[2]

                pindx = None
                if nTrials in pindx_cache:
                    pindx = pindx_cache[nTrials]
                else:
                    pindx = np.ndarray((comb(nTrials, 2), 2), dtype=int)
                    for k, e in enumerate(combinations(range(0, nTrials), 2)):
                        pindx[k, :] = e
                    pindx = np.transpose(np.concatenate((pindx, pindx[:, ::-1]), axis=0))
                    pindx_cache[nTrials] = pindx
                
                nPairs = pindx.shape[1]
                
                sxt = nSamples * nTrials
                thisVolume = np.transpose(thisVolume, (1, 0, 2))
                thisVolume = np.reshape(thisVolume, (nElectrodes, sxt))
                thisMu = np.nanmean(thisVolume, axis=1, dtype=np.float32)
                thisVolume = thisVolume - np.repeat(thisMu[:, np.newaxis], sxt, axis=1)
                thisVolume = np.reshape(thisVolume, (nElectrodes, nSamples, nTrials))
                
                sxp = nSamples * nPairs
                concatX = thisVolume[:, :, pindx[0, :]]
                concatY = thisVolume[:, :, pindx[1, :]]
                concatX = np.reshape(concatX, (nElectrodes, sxp))
                concatY = np.reshape(concatY, (nElectrodes, sxp))
                
                notNanX = ~np.isnan(concatX)
                notNanY = ~np.isnan(concatY)

                nPointsInXX[cond, subj] = np.matmul(notNanX, np.transpose(notNanX), dtype=int)
                nPointsInYY[cond, subj] = np.matmul(notNanY, np.transpose(notNanY), dtype=int)
                nPointsInXY[cond, subj] = np.matmul(notNanX, np.transpose(notNanY), dtype=int)

                np.nan_to_num(concatX, copy=False)
                np.nan_to_num(concatY, copy=False)

                sumXX[cond, subj] = np.matmul(concatX, np.transpose(concatX), dtype=np.float32)
                sumYY[cond, subj] = np.matmul(concatY, np.transpose(concatY), dtype=np.float32)
                sumXY[cond, subj] = np.matmul(concatX, np.transpose(concatY), dtype=np.float32)

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
                Y[c, s] = np.ndarray((data3D.shape[0], nComp, data3D.shape[2]), dtype=np.float32)
                nSamples, nElectrodes, nTrials = data3D.shape
                if nSamples < nElectrodes:
                    print("WARNING: number of samples is less than number of electrodes")
                if nElectrodes != W.shape[0]:
                    raise ValueError("number of electrodes in data and W must be the same")

                for comp in range(0, nComp):
                    rep = np.repeat(np.transpose(np.array(W[:, comp])), nTrials)
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
    def rcaRun(data: np.ndarray, nReg: int | None = None, nComp: int = 3, condRange: slice | None = None, subjRange: slice | None = None, cov_process_num: int = 1) -> Tuple[np.ndarray, np.matrix, np.matrix, np.matrix, np.ndarray, np.ndarray, np.ndarray, int]:
        if condRange is None:
            condRange = slice(0, data.shape[0])
        if subjRange is None:
            subjRange = slice(0, data.shape[1])
        sumXX, sumYY, sumXY, nPointsInXX, nPointsInYY, nPointsInXY = RCAModel.preComputeRcaCovariancesThread(data, condRange, subjRange, cov_process_num) if cov_process_num > 1 else RCAModel.preComputeRcaCovariances(data, condRange, subjRange)
        
        Rxx = np.sum(np.sum(sumXX, axis=1), axis=0) / np.sum(np.sum(nPointsInXX, axis=1), axis=0, dtype=np.float32)
        Ryy = np.sum(np.sum(sumYY, axis=1), axis=0) / np.sum(np.sum(nPointsInYY, axis=1), axis=0, dtype=np.float32)
        Rxy = np.sum(np.sum(sumXY, axis=1), axis=0) / np.sum(np.sum(nPointsInXY, axis=1), axis=0, dtype=np.float32)
        Wsub, A, _, dGenSort, K = RCAModel.rcaTrain(Rxx, Ryy, Rxy, nReg, nComp)
        Y = RCAModel.rcaProject(data, Wsub)
        return Y, Wsub, A, dGenSort, Rxx, Ryy, Rxy, K

    def rcaExplained(self, nInclude: int = 1) -> Tuple[np.ndarray, np.float32, np.ndarray, np.ndarray]:
        model = self
        pcaEigs, _ = np.linalg.eig(model.Rxx_ + model.Ryy_)
        pcaEigs = pcaEigs.astype(np.float32)
        pcaEigs_index = np.argsort(np.abs(pcaEigs), axis=0)
        pcaEigs_index = pcaEigs_index[::-1]
        pcaEigs = np.take_along_axis(pcaEigs, pcaEigs_index, axis=0)
        pcaVarExpl = np.sum(pcaEigs[0:nInclude], axis=0) / np.sum(pcaEigs, axis=0)
        
        rcaEigns = model.dGenSort_[::-1]
        rcaRelExpl = np.sum(rcaEigns[0:nInclude], axis=0) / np.sum(rcaEigns, axis=0)

        Rtotal = 0.5 * (model.Rxx_ + model.Ryy_)
        sigmas = np.diag(model.components_.T @ Rtotal @ model.components_) / np.diag(model.components_.T @ model.components_)
        rcaVarExpl = np.float32(np.sum(sigmas[0:nInclude]) / np.sum(sigmas))
        return rcaRelExpl, rcaVarExpl, pcaVarExpl, pcaEigs
    
def RCA() -> RCAModel:
    return RCAModel()


