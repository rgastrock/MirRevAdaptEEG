import pandas as pd
#import scipy as sp
import numpy as np
import time
import os
#import cProfile
#import re
import gc
from random import shuffle, seed
from psychopy import parallel, gui, visual, event, core, sound
from ctypes import *
#from Xlib import display

# Enable EEG triggers
# can switch to False if only behavioral experiment is needed
useTriggers = True
if useTriggers is True:
        parport = parallel.ParallelPort(address='/dev/parport0') 
        print("EEG trigger active.")

# dictionary mapping named codes to integers
#each participant gets a total of 30 triggers (9 trials, 9 tasks, 12 targets)
trigDictionary = {
        #trial triggers        
        "trial_onset": int(10),
        "target_onset": int(11),
        "go_target_onset": int(12),
        "reach_onset": int(13),
        "reach_end_feedback_onset": int(14), #reach end is when step moves to 5, which is also onset for feedback
        "feedback_home": int(15),
        "fixation_onset": int(16),
        #"fixation_task_end": int(18), #can remove because same as task end
        #task triggers
        "task_rest": int(20),
        "task_aligned": int(21),
        "task_random0": int(22),
        "task_rotation": int(23),
        "task_washout0": int(24),
        "task_random1": int(25),
        "task_mirror": int(26),
        "task_washout1": int(27),
        "task_end": int(28),
        #target locations based on participant condition (only get 12 of below)
        "target_q1_hor_near": int(30),
        "target_q1_hor_mid": int(31),
        "target_q1_hor_far": int(32),
        "target_q3_hor_near": int(33),
        "target_q3_hor_mid": int(34),
        "target_q3_hor_far": int(35),
        "target_q1_ver_near": int(36),
        "target_q1_ver_mid": int(37),
        "target_q1_ver_far": int(38),
        "target_q3_ver_near": int(39),
        "target_q3_ver_mid": int(40),
        "target_q3_ver_far": int(41),

        "target_q2_hor_near": int(50),
        "target_q2_hor_mid": int(51),
        "target_q2_hor_far": int(52),
        "target_q4_hor_near": int(53),
        "target_q4_hor_mid": int(54),
        "target_q4_hor_far": int(55),
        "target_q2_ver_near": int(56),
        "target_q2_ver_mid": int(57),
        "target_q2_ver_far": int(58),
        "target_q4_ver_near": int(59),
        "target_q4_ver_mid": int(60),
        "target_q4_ver_far": int(61),
}

def runExp():
        
        #set up config object
        cfg = {}

        #participant info
        cfg = getParticipantID(cfg)
        
        #set up psychopy environment:
        cfg = openEnvironment(cfg)

        cfg = makeStimuli(cfg)

        # set random number seed (so participants have different trial orders)
        seed(cfg['id']*999)

        cfg = makeBlocks(cfg)
        #loop through blocks
        for blockno in range(cfg['starting_block'],len(cfg['blocks'])):
                #print(cfg['blocks'][blockno]['instruction'])
                #print(cfg['blocks'][blockno]['trials'])

                cfg['taskno'] = blockno
                blockDef = cfg['blocks'][blockno]

                #if instruc not empty, show it
                if len(cfg['blocks'][blockno]['instruction']) > 0:
                        cfg = showInstruc(cfg)

                if blockDef['tasktype'] == 'resting state':
                        # do resting state stuff
                        eegTrigger(trigDictionary["task_rest"])
                        cfg['win'].flip()
                        triggerValue = int(0)
                        eegTrigger(triggerValue)
                        cfg = runRestState(cfg)

                else:
                        #loop through trials
                        ntrials = blockDef['trials'].shape[0]
                        if (cfg['mode']=='test'):
                                ntrials = 3

                        if blockDef['tasktype'] == 'aligned':
                                eegTrigger(trigDictionary["task_aligned"])
                        elif blockDef['tasktype'] == 'random0':
                                eegTrigger(trigDictionary["task_random0"])
                        elif blockDef['tasktype'] == 'washout0':
                                eegTrigger(trigDictionary["task_washout0"])
                        elif blockDef['tasktype'] == 'random1':
                                eegTrigger(trigDictionary["task_random1"])
                        elif blockDef['tasktype'] == 'washout1':
                                eegTrigger(trigDictionary["task_washout1"])
                        elif blockDef['tasktype'][:7] == 'perturb':
                                perturbtype = blockDef['trials'].iloc[0,2]
                                if perturbtype == 'rotation':
                                        eegTrigger(trigDictionary["task_rotation"])
                                else:
                                        eegTrigger(trigDictionary["task_mirror"])

                        cfg['win'].flip()
                        eegTrigger(int(0))                        

                        for trialno in range(ntrials):
                                cfg['trialno'] = trialno
                                #print(trialno)
                                cfg = runTrial(cfg)

                                gc.collect()
                        
                        #each trial is put into a csv file at first, eventually they grouped accdg. to task
                        taskdata = 0

                        for trialno in range(ntrials):
                        
                                filename = "p%03d-%d-%d.csv" % (cfg['id'], cfg['taskno'],trialno)
                                #print(filename)
                                filepath = os.path.join('data', filename)
                                #trialdata = pd.DataFrame.from_csv(path = filepath, index_col = None )
                                trialdata = pd.read_csv(filepath, index_col = None)
                                
                                if isinstance(taskdata, pd.DataFrame):
                                        taskdata = pd.concat([taskdata, trialdata])
                                else:
                                        taskdata = trialdata

                        #append whether it is a mirror/rotation block (perturbtype)
                        if blockDef['tasktype'][:7] == 'perturb':
                                perturbtype = blockDef['trials'].iloc[0,2]   #index perturbation type set to [0,2]                                            
                                filename = "p%03d-%d-%s.csv" % (cfg['id'], cfg['taskno'],perturbtype)
                        else:
                                filename = "p%03d-%d-%s.csv" % (cfg['id'], cfg['taskno'],blockDef['tasktype'])
                                
                        #print(filename)
                        filepath = os.path.join('data',filename)
                        taskdata.to_csv(filepath, index = False )
                        
                        trialdata = pd.DataFrame()
                        del trialdata
                        taskdata = pd.DataFrame()
                        del taskdata
                        gc.collect()
                eegTrigger(trigDictionary["task_end"])
                core.wait(0.036)
                eegTrigger(int(0))
        cfg = closeEnvironment(cfg)

def runRestState(cfg):

        # add fixation for resting state
        fixation = visual.ShapeStim(win = cfg['win'], vertices = ((0,-10),(0,10),(0,0),(-10,0),(10,0)),
                                    lineWidth=5,closeShape=False,lineColor="white")

        waitingForSpace = True
        eegTrigger(trigDictionary["fixation_onset"])
        while waitingForSpace: 
                triggerValue = 0
                pressed = event.getKeys(keyList = ['space'])
                fixation.draw()
                cfg['win'].flip()
                #eegTrigger(trigDictionary["fixation_onset"])
                eegTrigger(triggerValue)
                if len(pressed) > 0:
                        cfg['win'].flip()
                        #eegTrigger(trigDictionary["fixation_task_end"])
                        waitingForSpace = False

        
        return(cfg)

def eegTrigger(triggerType):
        if useTriggers is True:
                parport.setData(triggerType)
                #print (triggerType)
                #parport.readData()
                #core.wait(0.02)
                #parport.setData(0)
                #print(parport.readData())

def runTrial(cfg):
        blockno = cfg['taskno']
        trialno = cfg['trialno']
        blockDef = cfg['blocks'][blockno]

        trialDef = blockDef['trials'].iloc[trialno]
        
        target = trialDef.target
        
        #target triggers
        if target == 7.5:
                eegTrigger(trigDictionary["target_q1_hor_near"])
        elif target == 15:
                eegTrigger(trigDictionary["target_q1_hor_mid"])
        elif target == 22.5:
                eegTrigger(trigDictionary["target_q1_hor_far"])
        elif target == 187.5:
                eegTrigger(trigDictionary["target_q3_hor_near"])
        elif target == 195:
                eegTrigger(trigDictionary["target_q3_hor_mid"])
        elif target == 202.5:
                eegTrigger(trigDictionary["target_q3_hor_far"])
        elif target == 82.5:
                eegTrigger(trigDictionary["target_q1_ver_near"])
        elif target == 75:
                eegTrigger(trigDictionary["target_q1_ver_mid"])
        elif target == 67.5:
                eegTrigger(trigDictionary["target_q1_ver_far"])
        elif target == 262.5:
                eegTrigger(trigDictionary["target_q3_ver_near"])
        elif target == 255:
                eegTrigger(trigDictionary["target_q3_ver_mid"])
        elif target == 247.5:
                eegTrigger(trigDictionary["target_q3_ver_far"])
        elif target == 172.5:
                eegTrigger(trigDictionary["target_q2_hor_near"])
        elif target == 165:
                eegTrigger(trigDictionary["target_q2_hor_mid"])
        elif target == 157.5:
                eegTrigger(trigDictionary["target_q2_hor_far"])
        elif target == 352.5:
                eegTrigger(trigDictionary["target_q4_hor_near"])
        elif target == 345:
                eegTrigger(trigDictionary["target_q4_hor_mid"])
        elif target == 337.5:
                eegTrigger(trigDictionary["target_q4_hor_far"])
        elif target == 97.5:
                eegTrigger(trigDictionary["target_q2_ver_near"])
        elif target == 105:
                eegTrigger(trigDictionary["target_q2_ver_mid"])
        elif target == 112.5:
                eegTrigger(trigDictionary["target_q2_ver_far"])
        elif target == 277.5:
                eegTrigger(trigDictionary["target_q4_ver_near"])
        elif target == 285:
                eegTrigger(trigDictionary["target_q4_ver_mid"])
        elif target == 292.5:
                eegTrigger(trigDictionary["target_q4_ver_far"])
        cfg['win'].flip()
        eegTrigger(int(0))

        rotation = trialDef.rotation
        perturbation = trialDef.perturbation
        axis = trialDef.axis

        #define target positions
        theta = target * np.pi/180
        targetx = np.cos(theta)*cfg['targetdistance']
        targety = np.sin(theta)*cfg['targetdistance']
        targetx = targetx + cfg['home'].pos[0]
        targety = targety + cfg['home'].pos[1]

        cfg['targetgo'].pos = [targetx,targety]
        cfg['targetstay'].pos = [targetx,targety]
        #print(cfg['target'].pos)
        #cfg['cursor'].pos = [0,0]

        #define steps:
        # 0: reach home | only present for 1st trial of every block
        # 1: hold home for 300 ms
        # 2: planning - stay in position for 1 second, target revealed
        # 3: leave home
        # 4: reach target
        # 5: stay in position for 1 sec
        # 6: reach home
        # 7: end trial

        #set up variables for trial control:
        step = 0
        trialcorrect = float('NaN')
        #print(step)
        #create vectors to collect data samples:
        cursorx_px = []
        cursory_px = []
        mousex_px = []
        mousey_px = []
        time_ms = []
        step_vec = []
        correct = []
        #cursorendpt = []
        
        mousepos = cfg['mouse'].Pos()
        trial_start_time = mousepos[2]*1000

        #use below for setting up movement time constraints
        movetooslow = False
        moveduringerrorfeedback = False #set to true later if they do this then change color at last step when moving back home

        while (step < 7):
            triggerValue = 0
            # where would the cursor be?
            mousepos = cfg['mouse'].Pos()

            #include an escape option
            keys = event.getKeys()
            if keys:
                    if keys[0] == 'escape':
                            cfg['win'].close()
                            core.quit()

            #set cursor position if step 5
            if (step == 5):
                    cfg['cursor'].pos = cursorendpt
            else:
                    #for everything else, including perturbations
                    # get cursor position
                    cursorpos = mousepos[0:2]

                    cfg['cursor'].pos = cursorpos
                    #print(cursorpos)
                    #print(step)

                    if perturbation in ['rotation', 'random']:
                            #rotatepos function
                            cursorpos = rotatepos(point = cursorpos, ref = cfg['home'].pos, rotation = rotation) 
                    if perturbation == 'mirror':
                            curX = mousepos[0]
                            curY = mousepos[1]
                            #if diagonal is \ then mult x and y by neg 1, if diagonal is / then do just the flip
                            #why is this 45 and 135?
                            #if axis == 45: #diagonal is \
                            #        cursorpos = list(sp.flip([(curX*-1), (curY*-1)],0))
                            #elif axis == 135: #diagonal is /
                            #        cursorpos = list(sp.flip(cursorpos,0))
                            
                            #switch axis to cardinal directions
                            if axis == 0:
                                    cursorpos = list([(curX*-1),(curY)])
                            elif axis == 90:
                                    cursorpos = list([(curX),(curY*-1)])

##                    if curX >= 0 and curY > 0:
##                            cursorpos = list(sp.flip(cursorpos))
##                    elif curX <= 0 and curY < 0:
##                            cursorpos = list(sp.flip(cursorpos))
##                    elif curX >= 0 and curY < 0:
##                            cursorpos = list(sp.flip([(curX*-1), (curY*-1)]))
##                    elif curX <= 0 and curY > 0:
##                            cursorpos = list(sp.flip([(curX*-1), (curY*-1)]))
                    
                    cfg['cursor'].pos = cursorpos            
            
            if (step == 0):
                #reach home
                #need to bring cursor to home position
                # show home position and cursor
                cfg['home'].draw()
                cfg['cursor'].draw()
                # if distance between cursor and home position is lower than XYZ, move on to step 1 
                # a = cursorx - homex ; b = cursory - homey ; c^2 = a^2 + b^2; c = distance between cursor and home
                if np.sqrt((cursorpos[0] - cfg['home'].pos[0])**2 + (cursorpos[1] - cfg['home'].pos[1])**2) < cfg['radius']:
                        #get the time point after step 0
                        step0end = time.time()
                        step = 1
                        #eegTrigger(trigDictionary["trial_onset"])
                        triggerValue = trigDictionary['trial_onset']

            if (step == 1):
                #hold home for 300 ms
                cfg['home'].draw()
                cfg['cursor'].draw()
                
                # if distance between cursor and home position is lower than XYZ for 300 ms, move on to step 2
                # also needs to have been there for 300 ms
                #determine which samples to use
                #changed 100 to 300 below
                sample_idx = np.array([s >= (time_ms[-1]-300) for s in time_ms]).nonzero()[0]
                if (time.time() - step0end) > 0.3 and len(sample_idx) > 0:
                        curX = np.array(cursorx_px)[sample_idx] - cfg['home'].pos[0]
                        curY = np.array(cursory_px)[sample_idx] - cfg['home'].pos[1]

                        maxdist = max((curX**2 + curY**2)**0.5) #is this the within 0.5cm of home?

                        if maxdist < cfg['radius']:
                                step1end = time.time()
                                step = 2
                                #eegTrigger(trigDictionary["target_onset"])
                                triggerValue = trigDictionary['target_onset']
                else:
                        pass


            if (step == 2):

                #1 second wait
                #need to keep still for 1 second, otherwise will repeat step 1
                #show home and target
        
                cfg['home'].draw()
                cfg['cursor'].draw()
                cfg['targetstay'].draw()
                

                #want to control mouspos in X and Y directions
                #can probably make this more efficient?
                if mousepos[0] > (cfg['home'].pos[0] + cfg['radius']):
                        step = 1
                if mousepos[1] > (cfg['home'].pos[1] + cfg['radius']):
                        step = 1
                
                # if distance between cursor and home position is lower than XYZ for 1 sec, move to step 3
                sample_idx = np.array([s >= (time_ms[-1]-1000) for s in time_ms]).nonzero()[0]
                #move to step 2, how much time has passed since going to step 2
                #then check for distance to home for the last second (which is what you have right now)
                if (time.time() - step1end) > 1 and len(sample_idx) > 0:
                        curX = np.array(cursorx_px)[sample_idx] - cfg['home'].pos[0]
                        curY = np.array(cursory_px)[sample_idx] - cfg['home'].pos[1]
                        #print(curX)
                        #print(curY)
                        maxdist = max((curX**2 + curY**2)**0.5) #is this the within 0.5cm of home?
                        #print(maxdist)
                        if maxdist < cfg['radius']:
                                step2end = time.time()
                                step = 3
                                #eegTrigger(trigDictionary["go_target_onset"])
                                triggerValue = trigDictionary['go_target_onset']
                else:
                        pass                
            if (step == 3):
                #leave home
                #see target and leave home position

                        
                # show target
                cfg['targetgo'].draw()
                # show cursor
                cfg['cursor'].draw()
                   
                # if distance between cursor and home is higher than XYZ, move on to step 4
                if np.sqrt((cursorpos[0] - cfg['home'].pos[0])**2 + (cursorpos[1] - cfg['home'].pos[1])**2) > cfg['radius']:
                        #step3end = time.time()
                        step = 4
                        #eegTrigger(trigDictionary["reach_onset"])
                        triggerValue = trigDictionary['reach_onset']

            if (step == 4):
                #reach target
                # participant needs to reach towards distance of target

                # show target
                cfg['targetgo'].draw()
                # show cursor
                cfg['cursor'].draw()

                ##if distance between cursor and target is lower than XYZ, move on to step 5
                #if sp.sqrt((cursorpos[0] - cfg['targetgo'].pos[0])**2 + (cursorpos[1] - cfg['targetgo'].pos[1])**2) < cfg['radius']:
                #        step4end = time.time()
                #        step = 5

                #make a new condition, such that movement at a certain distance from home will be end of step
                #regardless of whether target was acquired
                #if distance between cursor and home is greater than or equal to target distance from home, move on to step 5
                if np.sqrt((cursorpos[0] - cfg['home'].pos[0])**2 + (cursorpos[1] - cfg['home'].pos[1])**2) >= np.sqrt((cfg['targetgo'].pos[0] - cfg['home'].pos[0])**2 + (cfg['targetgo'].pos[1] - cfg['home'].pos[1])**2):
                        cursorendpt = [cursorpos[0],cursorpos[1]]
                        mouseendpt = [mousepos[0],mousepos[1]]
                        #print(cursorendpt)
                        step4end = time.time()
                        #eegTrigger(trigDictionary["reach_end"])
                        step = 5
                        triggerValue = trigDictionary['reach_end_feedback_onset']
                        
            if (step == 5):
##                #1 second wait
##                #cursor position from step 4 is held there for 1 second
##                
##                #show home and target
##                cfg['targetgo'].draw()
##                cfg['cursor'].draw()
##                
##
##                # if distance between cursor and target is lower than XYZ for 1 sec, move to step 6
##                sample_idx = sp.array([s >= (time_ms[-1]-1000) for s in time_ms]).nonzero()[0]
##
##                if (time.time() - step4end) > 1 and len(sample_idx) > 0:
##                        curX = sp.array(cursorx_px)[sample_idx] - cfg['targetgo'].pos[0]
##                        curY = sp.array(cursory_px)[sample_idx] - cfg['targetgo'].pos[1]
##
##                        maxdist = max((curX**2 + curY**2)**0.5) #is this the within 0.5cm of home?
##
##                        if maxdist < cfg['radius']:
##                                step = 6
##                else:
##                        pass

                    
                #1 second wait
                #cursor position from step 4 is held there for 1 second

                #print (cursorendpt)
                #show home and target
                cfg['targetgo'].draw()
                cfg['cursor'].draw()
 
                # if distance between cursor and target is lower than XYZ for 1 sec, move to step 6
                sample_idx = np.array([s >= (time_ms[-1]-1000) for s in time_ms]).nonzero()[0]
                
                if (time.time() - step4end) > 1 and len(sample_idx) > 0:
                        #eegTrigger(trigDictionary["feedback_end"])
                        #triggerValue = trigDictionary['feedback_end']
                        mouseX = np.array(mousex_px)[sample_idx] - mouseendpt[0]
                        mouseY = np.array(mousey_px)[sample_idx] - mouseendpt[1]

                        maxdist = max((mouseX**2 + mouseY**2)**0.5) #is this the within 0.5cm of home?

                #also where you take into account feedback about movement time and whether they were moving while feedback was still
                        #what is an "adequate" distance to say that they have moved a little too much? Currently set to diameter of stimulus
                        #movement time from leaving home position to the when cursor stops should be less than 700ms
                        if maxdist > cfg['radius']*4 or not 0.4 <= (step4end - step2end) <= 0.7: #(step4end - step2end) > 0.7:
                                moveduringerrorfeedback = True
                                #print('too much movement')
                                movetooslow = True
                                #print(step4end-step2end)
                                #print('movement was too fast or slow')
                                #print(moveduringerrorfeedback)
                                #print(movetooslow)
                        cursorpos = mousepos #so that first sample in step 6 will be correct                      
                        step = 6
                        triggerValue = trigDictionary['feedback_home'] 

            #this will make next trial start from step 1
            #because this is the same as step 0
            #so only every first trial on every block will have step 0
            if (step == 6):
                #reach home
                # show home and cursor
                if moveduringerrorfeedback == True or movetooslow == True:
                        cfg['homefail'].draw()
                        cfg['cursor'].draw()
                        trialcorrect = 0
                else:
                        cfg['homesuccess'].draw()
                        cfg['cursor'].draw()
                        trialcorrect = 1
                # if distance between cursor and home is lower than XYZ, set step to 7
                if np.sqrt((cursorpos[0] - cfg['home'].pos[0])**2 + (cursorpos[1] - cfg['home'].pos[1])**2) < cfg['radius']:
                        step = 7
        


            cfg['win'].flip()
            eegTrigger(triggerValue)
            #print('window flipped')
            # add data to mouse X / Y vectors, cursor X / Y vectors, time vector
            cursorx_px.append(cursorpos[0]) 
            cursory_px.append(cursorpos[1])
            mousex_px.append(mousepos[0]) 
            mousey_px.append(mousepos[1])
            time_ms.append((mousepos[2]*1000)-trial_start_time)
            step_vec.append(step)
            correct.append(trialcorrect)

        d = {'step': step_vec,
             'time_ms': time_ms,
             'mousex_px': mousex_px,
             'mousey_px': mousey_px,
             'cursorx_px': cursorx_px,
             'cursory_px':cursory_px,
             'homex_px': cfg['home'].pos[0],
             'homey_px': cfg['home'].pos[1],
             'rotation': rotation,
             'participant': cfg['id'],
             'targetangle_deg': target,
             'targetx_px': cfg['targetgo'].pos[0],
             'targety_px': cfg['targetgo'].pos[1],
             'trial': cfg['trialno'],
             'trial_correct': correct}
             #'instruction':cfg['blocks'][cfg['blockno']]['instruction'][22:29]}

        trialdata = pd.DataFrame(d)
        filename = "p%03d-%d-%d.csv" % (cfg['id'],cfg['taskno'],cfg['trialno'])
        filepath = os.path.join('data', filename)

        trialdata.to_csv(filepath, index=False)
        
        del cursorx_px
        del cursory_px
        del mousex_px
        del mousey_px
        del time_ms
        del step_vec
        del correct
        
        trialdata = pd.DataFrame()
        del trialdata     

        return(cfg)


def getParticipantID(cfg):
        validID = False
        while not(validID):
                #print(validID)
                str_id = input('participant ID (integer): ')
                int_id = int(str_id)
                
                str_sb = input('starting block (integer): ')
                int_sb = int(str_sb)

                str_tm = input('mode: ') #'test' or 'real or even just enter'
                
                if '%d'%int_id == str_id and '%d'%int_sb == str_sb:
                        validID = True
    

        cfg['id'] = int_id
        cfg['starting_block'] = int_sb
        cfg['mode'] = str_tm
        return(cfg)
                                      
def showInstruc(cfg):
        # print(cfg['blocks'][cfg['blockno']]['instruction'])
        # show this on screen!
        
       
        instruction = visual.TextStim(cfg['win'], text = cfg['blocks'][cfg['taskno']]['instruction'], height = 16)
        waitingForSpace = True
        
        while waitingForSpace: 
                pressed = event.getKeys(keyList = ['space'])
                instruction.draw()
                cfg['win'].flip()
                if len(pressed) > 0:
                        cfg['win'].flip()
                        waitingForSpace = False

        
        return(cfg)

def openEnvironment(cfg):
        
        # 47.4 * 29.6 
        
        # 1680 / 47.4 ~ 34.443 pix per cm
        # 1050 / 29.6 ~ 35.4729 pix per cm
        
        # let's say 35 pix / cm?

        #eeg room monitor: 36.5 * 27.4
        #1024/36.5 = 28.055
        #768/27.4 = 28.029
        #around 28 pix/cm?
        
        #keep in mind that conversion and winSize will change depending on monitor. change accordingly
        #cfg['pixpercm'] = 35
        cfg['pixpercm'] = 28.04

        #winSize = [1350, 750]
        #winSize = [640,480]
        winSize = [1024,768] #for this monitor
        #winSize = [1680, 1050]
        
        cfg['win'] = visual.Window(size = winSize, color =(0,0,0), units ='pix', fullscr=True, winType = 'pyglet') #, gamma=256)
        
        
        cfg['winSize'] = winSize
        
        cfg['psyMouse'] = event.Mouse(visible = False, newPos = None, win = cfg['win'])
        
        cfg = addMouse(cfg)
        
        return(cfg)

##def defineMouse(cfg):
##
##        try:
##
##                class myMouse:
##                        Xlib = CDLL("libX11.so.6")
##                        display = Xlib.XOpenDisplay(None)
##                        if display == 0: sys.exit(2) # no display or can't be accessed...
##                        w = Xlib.XRootWindow(display, c_int(cfg['monitorIndex']-1))
##                        (root_id, child_id) = (c_uint32(), c_uint32())
##                        (root_x, root_y, win_x, win_y) = (c_int(), c_int(), c_int(), c_int())
##                        mask = c_uint()
##                              
##                        def Pos(self):
##                                print('X11 mouse')
##                                ret = self.Xlib.XQueryPointer(self.display, c_uint32(self.w), byref(self.root_id), byref(self.child_id), byref(self.root_x), byref(self.root_y), byref(self.win_x), byref(self.win_y), byref(self.mask))
##                                if ret == 0: sys.exit(1)
##                                return [(self.root_x.value - (cfg['width']/2))/(1.6*1.1), -1 * (self.root_y.value - (cfg['height']/2))/1.1, time.time()] # c_int can't be used by regular Python to do math, but the values of c_ints are ints - also, we return the current time
##
##        except:
##                print('PsychoPy mouse')
##                    # Xlib is not available (not on Linux?)
##                    # use Psychopy:
##                class myMouse:
##                  
##                        def Pos(self):
##                                #print('PsychoPy mouse')
##                                [X,Y] = cfg['psyMouse'].getPos()
##                                return [X/(1.6/1.05),Y*1.05,time.time()]
##          
##        cfg['mouse'] = myMouse()
##        return(cfg)

def addMouse(cfg):
        #this should always be done?        
        cfg['psyMouse'] = event.Mouse(visible = False, newPos = None, win = cfg['win'])

#        try:
#        # the X coordinate is scaled, so that on the 16:10 widescreen monitor, and square tablet, reaches are still proportional
#        # factor: 1.6
#        # both X and Y are then scaled up, so that the movement in centimeters is equal on the screen and tablet
#        # factor: 1.05
#        # we try to use one of the X11 ways to create a mouse object, because the time sampling is more accurate
#        #there are two ways to do this, the first one is simpler and works in Python 3.X
        
#                try:
#                       class myMouse:
#                        #from Xlib import display - this needs to be on top
#
#                              def Pos(self):
#                                        #print('A X11 mouse')
#                                        #print(cfg.keys())                                        
#                                     qp = display.Display().screen().root.query_pointer()
#                                    return [(qp.root_x - (cfg['winSize'][0]/2))/(1.6*1.1), -1* (qp.root_y - (cfg['winSize'][1]/2)/1.1), time.time()] #winSize instead of width and height
#
#           except:
#                        #if the simpler method does not work, we try the harder one:
#                  class myMouse:
#                         Xlib = CDLL("libX11.so.6")
#                        display = Xlib.XOpenDisplay(None)
#                       if display == 0: sys.exit(2) # no display or can't be accessed...
#                      w = Xlib.XRootWindow(display, c_int(0)) #c_int(cfg['monitorIndex']-1)); but monitorIndex does not exist
#                     (root_id, child_id) = (c_uint32(), c_uint32())
#                    (root_x, root_y, win_x, win_y) = (c_int(), c_int(), c_int(), c_int())
#                   mask = c_uint()
#                
#                 def Pos(self):
#                                        #print('X11 mouse')
#                        ret = self.Xlib.XQueryPointer(self.display, c_uint32(self.w), byref(self.root_id), byref(self.child_id), byref(self.root_x), byref(self.root_y), byref(self.win_x), byref(self.win_y), byref(self.mask))
#                       if ret == 0: sys.exit(1)
#                      return [(self.root_x.value - (cfg['winSize']/2))/(1.6*1.1), -1 * (self.root_y.value - (cfg['winSize']/2))/1.1, time.time()] # c_int can't be used by regular Python to do math, but the values of c_ints are ints - also, we return the current time
          
        #except:
        #        print('PsychoPy mouse')
                # if the X11 methods don't seem to work, create an identically named object with identical behaviour based on PsychoPy
        class myMouse:
                  
            def Pos(self):
                #print('PsychoPy mouse')
                [X,Y] = cfg['psyMouse'].getPos() #get time here to compare with lower, then add to output later on as separate column
                #kX = 0.5*cfg['radius']
                #kY = 1.5*cfg['radius']
                #return [(X/(1.6/1.05))-kX,(Y*0.98)-kY,time.time()] #0.95 is really close too for Y               
                #kX (0.5*) and kY (1.5*)are offsets to recenter the circle in tablet (10 cm radius)
                #1.6 is a factor that makes the square for the monitor (tablet is square, monitor is not)
                #1.05 and 0.98 are trial and error factors that help to fit the stencil appropriately                
                return [X,Y,time.time()]
                #tablet is no longer a square, so no need for adjustments

        cfg['mouse'] = myMouse()
        
        return(cfg)


def closeEnvironment(cfg):
        
        cfg['psyMouse'].setVisible(True)
        cfg['win'].close() 
        return(cfg)

def makeStimuli(cfg):
        
        radius = 0.5*cfg['pixpercm'] #0.0375*2/3*cfg['winSize'][1]
        lineWidth = 4
        cfg['radius'] = radius

        # add home position (grey, darker than background (which is roughly 128,128,128))
        cfg['home'] = visual.Circle(win = cfg['win'], pos = [0,0], radius = radius, lineWidth = lineWidth)
        cfg['home'].setFillColor(color=(128,128,128), colorSpace = 'rgb255')
        cfg['home'].setLineColor(color=(64,64,64), colorSpace = 'rgb255')

        # add home fail feedback (red)
        cfg['homefail'] = visual.Circle(win = cfg['win'], pos = [0,0], radius = radius, lineWidth = lineWidth)
        cfg['homefail'].setFillColor(color=(128,128,128), colorSpace = 'rgb255')
        cfg['homefail'].setLineColor(color=(255,0,0), colorSpace = 'rgb255')

        # add home success feedback (blue)
        cfg['homesuccess'] = visual.Circle(win = cfg['win'], pos = [0,0], radius = radius, lineWidth = lineWidth)
        cfg['homesuccess'].setFillColor(color=(128,128,128), colorSpace = 'rgb255')
        cfg['homesuccess'].setLineColor(color=(0,0,255), colorSpace = 'rgb255')

        # add target stay (grey, darker than bg)
        cfg['targetstay'] = visual.Circle(win = cfg['win'], pos = [0,-0.25*cfg['winSize'][1]], radius = radius, lineWidth = lineWidth)
        cfg['targetstay'].setFillColor(color=(128,128,128), colorSpace = 'rgb255')
        cfg['targetstay'].setLineColor(color=(64,64,64), colorSpace = 'rgb255')
        
        # add target go (dark blue)
        cfg['targetgo'] = visual.Circle(win = cfg['win'], pos = [0,-0.25*cfg['winSize'][1]], radius = radius, lineWidth = lineWidth)
        cfg['targetgo'].setFillColor(color=(128,128,128), colorSpace = 'rgb255')
        cfg['targetgo'].setLineColor(color=(0,0,255), colorSpace = 'rgb255')

        #would want the targetdistance to be set, maybe 40% of distance from home to edge of window
        if (cfg['winSize'][1] == 1050):
                cfg['targetdistance'] = cfg['pixpercm'] * 9 #12. 9 because stencil radius is 10
        else:
                cfg['targetdistance'] = cfg['pixpercm'] * 5 #5
        
        # add cursor (white)
        cfg['cursor'] = visual.Circle(win = cfg['win'], pos = [0,-0.25*cfg['winSize'][1]], radius = radius, lineWidth = lineWidth)
        cfg['cursor'].setFillColor(color=(128,128,128), colorSpace = 'rgb255')
        cfg['cursor'].setLineColor(color=(255,255,255), colorSpace = 'rgb255')



        #most likely will not need sound
        #add Beep sound
        #sound.init(rate=44100, stereo=True, buffer=128)
        #cfg['sound'] = sound.Sound('ding.wav', secs = 1)
        
        return(cfg)


def makeBlocks(cfg):

        # empty list with blocks to do:
        blocks = []

        #set properties of blocks:
        tasks = ['resting state', 'aligned', 'resting state', 'random0', 'resting state', 'perturb0',
                 'resting state', 'washout0', 'resting state', 'random1', 'resting state', 'perturb1',
                 'resting state', 'washout1', 'resting state']
        
        #rot_axis = [45,45,45,45,135,135,135,135]
        rot_axis = [0,0,0,0,90,90,90,90]        
        rot_dir = [1,1,-1,-1,1,1,-1,-1]
        rot_place = [1,2,1,2,1,2,1,2]

        #conditions = pd.DataFrame({'rot_axis' : rot_axis, 'rot_dir' : rot_dir, 'rot_place' : rot_place})

        #index by participant number
        orderIndex = cfg['id'] % 8 #0,1,2,3,4,5,6,7; 8
        
        pp_rot_axis = rot_axis[orderIndex]
        pp_rot_dir = rot_dir[orderIndex]
        pp_rot_place = rot_place[orderIndex]
        
        #ppcond = conditions.iloc(orderIndex) #replaced irow with iloc get a row from df depending on index
        #print(conditions)
        #all tasks are fixed but we need to counterbalance between rot and mir
        orders = {'1':['rotation','mirror'], '2':['mirror','rotation']}

        pptaskorder = orders['%d'%pp_rot_place]

        

        #pptaskorder[0] #0 or 1 for value here

        #generate instructions before each task
        instruct = ['resting state', 'reach with cursor', 'resting state', 'reach with cursor', 'resting state', 'reach with cursor',
                 'resting state', 'reach with cursor', 'resting state', 'reach with cursor', 'resting state', 'reach with cursor',
                 'resting state', 'reach with cursor', 'resting state']
        
        trialTargets = np.array([7.5,15,22.5,187.5,195,202.5])
        
        taskBlockNos = [0,4,0,8,0,15,0,8,0,8,0,15,0,8,0] #e.g. 8 times 6 trials in a block
        # tasks with 0 blocks actually are resting state tasks (i.e. no blocks is correct)
        
        taskRotations = [0,0,0,[-35,-25,-15,15,25,35],0,30,0,0,0,[-35,-25,-15,15,25,35],0,30,0,0,0]
        
        for taskno in range(len(tasks)):

                task = tasks[taskno]
                
                blockDef = {}
                blockDef['instruction'] = instruct[taskno]

                target = []
                rotation = []
                perturbation = []
                axis = []

                if task == 'resting state':
                        # do nothing ? table not required and instruction is above
                        pass
                else:
                        #essentially, we want a table with trials
                        #6 trials per block
                        #would need to shuffle per block (trials will be 48)
                        #so there will be 8 sets (8*6=48) or equivalent for perturb
                        for trialSet in range(taskBlockNos[taskno]):
                                trialindices = [0,1,2,3,4,5]
                                shuffle(trialindices)
                                #then we are creating lists and putting them on a df later
                                target = target + list(trialTargets[trialindices])
                                if isinstance(taskRotations[taskno],list):
                                        shuffle(trialindices)
                                        rotation = rotation + list(np.array(taskRotations[taskno])[trialindices])
                                        perturbation = ['random'] * len(target)
                                        axis = [float('NaN')] * len(target)
                                else:
                                        rotation = rotation + [taskRotations[taskno]] * len(trialindices)
                                        
                        taskPerturbation = 'none'
                        if task[:6] == 'random':
                                taskPerturbation = pptaskorder[int(task[6])]
                                if taskPerturbation == 'rotation':
                                        direction = pp_rot_dir
                                        taskaxis = pp_rot_axis
                                        #axis = [float('NaN')] * len(target)
                                else:
                                        direction = -1 * pp_rot_dir
                                        taskaxis = (-1 * pp_rot_axis) + 90 #this is the reason why axis specified for diagonal is reversed
                                        #axis = [pp_rot_axis] * len(target) 

                                if direction == -1:
                                        target = [target[ind] - 30 for ind in range(len(target))] #not accdg to random rotation because rotation should be 30 for mirror
                                target = list((np.array(target) + taskaxis)%360)
        
                        taskPerturbation = 'none'
                        
                        if task[:7] == 'perturb':
                                taskPerturbation = pptaskorder[int(task[7])]
                                if taskPerturbation == 'rotation':
                                        direction = pp_rot_dir
                                        taskaxis = pp_rot_axis
                                        axis = [float('NaN')] * len(target)
                                else:
                                        direction = -1 * pp_rot_dir
                                        taskaxis = (-1 * pp_rot_axis) + 90 #this is the reason why axis specified for diagonal is reversed
                                        axis = [pp_rot_axis] * len(target) 

                                if direction == -1:
                                        target = [target[ind] - rotation[ind] for ind in range(len(target))]
                                target = list((np.array(target) + taskaxis)%360)
                                
                                perturbation = [taskPerturbation] * len(target)
##                                print(axis)
##                                print(target)
##                                print(rotation)

                        if task [:-1] == 'washout':
                                #same as perturbation? since 6 targets only?
                                #but we don't want the rotation
                                taskPerturbation = pptaskorder[int(task[7])]
                                if taskPerturbation == 'rotation':
                                        direction = pp_rot_dir
                                        taskaxis = pp_rot_axis
                                else:
                                        direction = -1 * pp_rot_dir
                                        taskaxis = (-1 * pp_rot_axis) + 90

                                if direction == -1:
                                        target = [target[ind] - 30 for ind in range(len(target))] #need to have the 30 deg rotation in there, not zero
                                target = list((np.array(target) + taskaxis)%360)
                                #rotation = [0] * len(target)
                                perturbation = ['none'] * len(target)
                                axis = [float('NaN')] * len(target)
##                                print(axis)
##                                print(target)
##                                print(rotation)
                        if task == 'aligned':
                                targets = list(np.array(trialTargets) + 0) + list((np.array(trialTargets) - 30) + 90) #+ list(sp.array(trialTargets) + 135)
                                #print(target)
                                target = []

                                if pp_rot_axis == 90:
                                        targets = list((np.array(targets) + 90) % 360) #modulo: so that after 360 deg, it returns to 0
                                        
                                if pp_rot_dir == -1:
                                        targets = list((np.array(targets) + 90) % 360)
                                trialSets = taskBlockNos[taskno]
                                for trialSet in range(taskBlockNos[taskno]):
                                        trialindices = list(np.arange(len(targets)))#array range
                                        shuffle(trialindices)
                                        #print(trialindices)
                                        #then we are creating lists and putting them on a df later
                                        target = target + list(np.array(targets)[trialindices])
                                rotation = [0] * len(target)
                                perturbation = ['none'] * len(target)
                                axis = [float('NaN')] * len(target)
                                

                                
                #convert block lists to df:
                #print(len(rotation))
                #print(len(target))
                #print(len(perturbation))
                trialDefs = pd.DataFrame({'target':target, 'rotation':rotation, 'perturbation':perturbation, 'axis':axis})
                blockDef['trials'] = trialDefs
                blockDef['tasktype'] = task
                #add it to the list of block definitions:
                blocks.append(blockDef)

        cfg['blocks'] = blocks
        return(cfg)



def rotatepos(point, ref=[0,0], rotation = 0):
        
        t = rotation*np.pi/180
        
        R = np.array([[np.cos(t), -1*np.sin(t)], [np.sin(t), np.cos(t)]])
        
        point = np.array(point) - np.array(ref)
        
        return(list(np.dot(R,point) + np.array(ref)))


runExp()
#cProfile.run('runExp()')
