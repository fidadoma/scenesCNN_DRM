#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
This experiment was created using PsychoPy3 Experiment Builder (v3.0.7),
    on duben 24, 2019, at 18:54
If you publish work using this script please cite the PsychoPy publications:
    Peirce, JW (2007) PsychoPy - Psychophysics software in Python.
        Journal of Neuroscience Methods, 162(1-2), 8-13.
    Peirce, JW (2009) Generating stimuli for neuroscience using PsychoPy.
        Frontiers in Neuroinformatics, 2:10. doi: 10.3389/neuro.11.010.2008
"""

from __future__ import absolute_import, division
from psychopy import locale_setup, sound, gui, visual, core, data, event, logging, clock
from psychopy.constants import (NOT_STARTED, STARTED, PLAYING, PAUSED,
                                STOPPED, FINISHED, PRESSED, RELEASED, FOREVER)
import numpy as np  # whole numpy lib is available, prepend 'np.'
from numpy import (sin, cos, tan, log, log10, pi, average,
                   sqrt, std, deg2rad, rad2deg, linspace, asarray)
from numpy.random import random, randint, normal, shuffle
import os  # handy system and path functions
import sys  # to get file system encoding


# Ensure that relative paths start from the same directory as this script
_thisDir = os.path.dirname(os.path.abspath(__file__))
os.chdir(_thisDir)

# Store info about the experiment session
psychopyVersion = '3.0.7'
expName = 'scenesDRM'  # from the Builder filename that created this script
expInfo = {'participant': '1'}
dlg = gui.DlgFromDict(dictionary=expInfo, title=expName)
if dlg.OK == False:
    core.quit()  # user pressed cancel
expInfo['date'] = data.getDateStr()  # add a simple timestamp
expInfo['expName'] = expName
expInfo['psychopyVersion'] = psychopyVersion

# Data file name stem = absolute path + name; later add .psyexp, .csv, .log, etc
filename = _thisDir + os.sep + u'data' + os.sep + '%s_%s' % (expInfo['participant'], expInfo['date'])

# An ExperimentHandler isn't essential but helps with data saving
thisExp = data.ExperimentHandler(name=expName, version='',
    extraInfo=expInfo, runtimeInfo=None,
    originPath='D:\\Documents\\git\\scenesCNN_DRM\\psychopy\\DRM_exp\\DRM_exp1.py',
    savePickle=True, saveWideText=True,
    dataFileName=filename)
# save a log file for detail verbose info
logFile = logging.LogFile(filename+'.log', level=logging.EXP)
logging.console.setLevel(logging.WARNING)  # this outputs to the screen, not a file

endExpNow = False  # flag for 'escape' or other condition => quit the exp

# Start Code - component code to be run before the window creation

# Setup the Window
win = visual.Window(
    size=[1920, 1200], fullscr=True, screen=0,
    allowGUI=True, allowStencil=False,
    monitor='testMonitor', color=[0.000,0.000,0.000], colorSpace='rgb',
    blendMode='avg', useFBO=True)
# store frame rate of monitor if we can measure it
expInfo['frameRate'] = win.getActualFrameRate()
if expInfo['frameRate'] != None:
    frameDur = 1.0 / round(expInfo['frameRate'])
else:
    frameDur = 1.0 / 60.0  # could not measure, so guess

# Initialize components for Routine "welcome"
welcomeClock = core.Clock()
text = visual.TextStim(win=win, name='text',
    text='Memory for photographs',
    font='Arial',
    units='norm', pos=[0, .5], height=0.1, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=0.0);
text_2 = visual.TextStim(win=win, name='text_2',
    text='In this experiment, you will be presented with photographs and your task is memorize them as accurately as possible. \n \nPress Space to continue',
    font='Arial',
    units='norm', pos=[0, -.10], height=0.05, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-2.0);

# Initialize components for Routine "instructions"
instructionsClock = core.Clock()
text_3 = visual.TextStim(win=win, name='text_3',
    text='Photographs will be displayed in sequence. After 15 scenes, 15 photos will be displayed and your task will be to decide whether you saw each photo (left arrow) or not (right arrow). Then you will rate how sure you are with your response\n \nEach photo will be presented for 1 second\n \nPress Space to continue',
    font='Arial',
    pos=[0, 0], height=0.05, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=0.0);
protId= 1 +((int(expInfo['participant']) - 1) % 20)

protocolFile="protocols/P%03d.csv" % (protId)


# Initialize components for Routine "select_trial"
select_trialClock = core.Clock()

# Initialize components for Routine "category_start"
category_startClock = core.Clock()
text_8 = visual.TextStim(win=win, name='text_8',
    text='default text',
    font='Arial',
    pos=(0, 0), height=0.1, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=0.0);

# Initialize components for Routine "trial"
trialClock = core.Clock()
imagex = visual.ImageStim(
    win=win,
    name='imagex', units='pix', 
    image='sin', mask=None,
    ori=0, pos=[0, 0], size=[700,700],
    color=[1,1,1], colorSpace='rgb', opacity=1,
    flipHoriz=False, flipVert=False,
    texRes=128, interpolate=True, depth=0.0)

# Initialize components for Routine "vigilance"
vigilanceClock = core.Clock()
image_query = visual.ImageStim(
    win=win,
    name='image_query', units='pix', 
    image='sin', mask=None,
    ori=0, pos=(0, 0), size=(700, 700),
    color=[1,1,1], colorSpace='rgb', opacity=1,
    flipHoriz=False, flipVert=False,
    texRes=128, interpolate=True, depth=0.0)
text_11 = visual.TextStim(win=win, name='text_11',
    text='old',
    font='Arial',
    pos=(-0.5, -0.85), height=0.1, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-2.0);
text_12 = visual.TextStim(win=win, name='text_12',
    text='new',
    font='Arial',
    pos=(0.5, -0.85), height=0.1, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-3.0);

# Initialize components for Routine "vigilance_rating"
vigilance_ratingClock = core.Clock()
image_query2 = visual.ImageStim(
    win=win,
    name='image_query2', units='pix', 
    image='sin', mask=None,
    ori=0, pos=(0, 0), size=(700, 700),
    color=[1,1,1], colorSpace='rgb', opacity=1,
    flipHoriz=False, flipVert=False,
    texRes=128, interpolate=True, depth=0.0)
rating = visual.RatingScale(win=win, name='rating', ratingScale = visual.RatingScale(win,choices=['not at all sure','somewhat sure','very sure'], markerStart=2,leftKeys='left',rightKeys = 'right',acceptKeys='down')

# Initialize components for Routine "thanks"
thanksClock = core.Clock()
text_4 = visual.TextStim(win=win, name='text_4',
    text='default text',
    font='Arial',
    pos=[0, 0.2], height=0.1, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=0.0);

# Create some handy timers
globalClock = core.Clock()  # to track the time since experiment started
routineTimer = core.CountdownTimer()  # to track time remaining of each (non-slip) routine 

# ------Prepare to start Routine "welcome"-------
t = 0
welcomeClock.reset()  # clock
frameN = -1
continueRoutine = True
# update component parameters for each repeat
key_resp_2 = event.BuilderKeyResponse()

lastquery=1

# keep track of which components have finished
welcomeComponents = [text, key_resp_2, text_2]
for thisComponent in welcomeComponents:
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED

# -------Start Routine "welcome"-------
while continueRoutine:
    # get current time
    t = welcomeClock.getTime()
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *text* updates
    if t >= 0.0 and text.status == NOT_STARTED:
        # keep track of start time/frame for later
        text.tStart = t
        text.frameNStart = frameN  # exact frame index
        text.setAutoDraw(True)
    
    # *key_resp_2* updates
    if t >= 0.0 and key_resp_2.status == NOT_STARTED:
        # keep track of start time/frame for later
        key_resp_2.tStart = t
        key_resp_2.frameNStart = frameN  # exact frame index
        key_resp_2.status = STARTED
        # keyboard checking is just starting
        event.clearEvents(eventType='keyboard')
    if key_resp_2.status == STARTED:
        theseKeys = event.getKeys(keyList=['space'])
        
        # check for quit:
        if "escape" in theseKeys:
            endExpNow = True
        if len(theseKeys) > 0:  # at least one key was pressed
            # a response ends the routine
            continueRoutine = False
    
    # *text_2* updates
    if t >= 0.0 and text_2.status == NOT_STARTED:
        # keep track of start time/frame for later
        text_2.tStart = t
        text_2.frameNStart = frameN  # exact frame index
        text_2.setAutoDraw(True)
    
    # check for quit (typically the Esc key)
    if endExpNow or event.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in welcomeComponents:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "welcome"-------
for thisComponent in welcomeComponents:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
# the Routine "welcome" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# ------Prepare to start Routine "instructions"-------
t = 0
instructionsClock.reset()  # clock
frameN = -1
continueRoutine = True
# update component parameters for each repeat
key_resp_3 = event.BuilderKeyResponse()
# keep track of which components have finished
instructionsComponents = [text_3, key_resp_3]
for thisComponent in instructionsComponents:
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED

# -------Start Routine "instructions"-------
while continueRoutine:
    # get current time
    t = instructionsClock.getTime()
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *text_3* updates
    if t >= 0.0 and text_3.status == NOT_STARTED:
        # keep track of start time/frame for later
        text_3.tStart = t
        text_3.frameNStart = frameN  # exact frame index
        text_3.setAutoDraw(True)
    
    # *key_resp_3* updates
    if t >= 0.0 and key_resp_3.status == NOT_STARTED:
        # keep track of start time/frame for later
        key_resp_3.tStart = t
        key_resp_3.frameNStart = frameN  # exact frame index
        key_resp_3.status = STARTED
        # keyboard checking is just starting
        event.clearEvents(eventType='keyboard')
    if key_resp_3.status == STARTED:
        theseKeys = event.getKeys(keyList=['space'])
        
        # check for quit:
        if "escape" in theseKeys:
            endExpNow = True
        if len(theseKeys) > 0:  # at least one key was pressed
            # a response ends the routine
            continueRoutine = False
    
    # check for quit (typically the Esc key)
    if endExpNow or event.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in instructionsComponents:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "instructions"-------
for thisComponent in instructionsComponents:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
# the Routine "instructions" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# set up handler to look after randomisation of conditions etc
trialsA = data.TrialHandler(nReps=1, method='sequential', 
    extraInfo=expInfo, originPath=-1,
    trialList=data.importConditions(protocolFile),
    seed=None, name='trialsA')
thisExp.addLoop(trialsA)  # add the loop to the experiment
thisTrialsA = trialsA.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb = thisTrialsA.rgb)
if thisTrialsA != None:
    for paramName in thisTrialsA:
        exec('{} = thisTrialsA[paramName]'.format(paramName))

for thisTrialsA in trialsA:
    currentLoop = trialsA
    # abbreviate parameter names if possible (e.g. rgb = thisTrialsA.rgb)
    if thisTrialsA != None:
        for paramName in thisTrialsA:
            exec('{} = thisTrialsA[paramName]'.format(paramName))
    
    # ------Prepare to start Routine "select_trial"-------
    t = 0
    select_trialClock.reset()  # clock
    frameN = -1
    continueRoutine = True
    # update component parameters for each repeat
    if query != lastquery and lastquery == 1:
        lastquery = query
        startTxt = category
        txtDur = 2
    else:
        startTxt = '+'
        txtDur = 0.3
    
    if  query == 0:
        showImage = 1
        showQuery  = 0
    else:
        lastquery = 1
        showImage = 0
        showQuery  = 1
    # keep track of which components have finished
    select_trialComponents = []
    for thisComponent in select_trialComponents:
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    
    # -------Start Routine "select_trial"-------
    while continueRoutine:
        # get current time
        t = select_trialClock.getTime()
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # check for quit (typically the Esc key)
        if endExpNow or event.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in select_trialComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "select_trial"-------
    for thisComponent in select_trialComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # the Routine "select_trial" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # ------Prepare to start Routine "category_start"-------
    t = 0
    category_startClock.reset()  # clock
    frameN = -1
    continueRoutine = True
    # update component parameters for each repeat
    text_8.setText(startTxt)
    # keep track of which components have finished
    category_startComponents = [text_8]
    for thisComponent in category_startComponents:
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    
    # -------Start Routine "category_start"-------
    while continueRoutine:
        # get current time
        t = category_startClock.getTime()
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *text_8* updates
        if t >= 0.0 and text_8.status == NOT_STARTED:
            # keep track of start time/frame for later
            text_8.tStart = t
            text_8.frameNStart = frameN  # exact frame index
            text_8.setAutoDraw(True)
        frameRemains = 0.0 + txtDur- win.monitorFramePeriod * 0.75  # most of one frame period left
        if text_8.status == STARTED and t >= frameRemains:
            text_8.setAutoDraw(False)
        
        # check for quit (typically the Esc key)
        if endExpNow or event.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in category_startComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "category_start"-------
    for thisComponent in category_startComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # the Routine "category_start" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # set up handler to look after randomisation of conditions etc
    should_image_be_shown = data.TrialHandler(nReps=showImage, method='sequential', 
        extraInfo=expInfo, originPath=-1,
        trialList=[None],
        seed=None, name='should_image_be_shown')
    thisExp.addLoop(should_image_be_shown)  # add the loop to the experiment
    thisShould_image_be_shown = should_image_be_shown.trialList[0]  # so we can initialise stimuli with some values
    # abbreviate parameter names if possible (e.g. rgb = thisShould_image_be_shown.rgb)
    if thisShould_image_be_shown != None:
        for paramName in thisShould_image_be_shown:
            exec('{} = thisShould_image_be_shown[paramName]'.format(paramName))
    
    for thisShould_image_be_shown in should_image_be_shown:
        currentLoop = should_image_be_shown
        # abbreviate parameter names if possible (e.g. rgb = thisShould_image_be_shown.rgb)
        if thisShould_image_be_shown != None:
            for paramName in thisShould_image_be_shown:
                exec('{} = thisShould_image_be_shown[paramName]'.format(paramName))
        
        # ------Prepare to start Routine "trial"-------
        t = 0
        trialClock.reset()  # clock
        frameN = -1
        continueRoutine = True
        routineTimer.add(1.000000)
        # update component parameters for each repeat
        imagex.setImage("stimuli_exp1/"+img_name)
        # keep track of which components have finished
        trialComponents = [imagex]
        for thisComponent in trialComponents:
            if hasattr(thisComponent, 'status'):
                thisComponent.status = NOT_STARTED
        
        # -------Start Routine "trial"-------
        while continueRoutine and routineTimer.getTime() > 0:
            # get current time
            t = trialClock.getTime()
            frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
            # update/draw components on each frame
            
            # *imagex* updates
            if t >= 0.0 and imagex.status == NOT_STARTED:
                # keep track of start time/frame for later
                imagex.tStart = t
                imagex.frameNStart = frameN  # exact frame index
                imagex.setAutoDraw(True)
            frameRemains = 0.0 + 1.0- win.monitorFramePeriod * 0.75  # most of one frame period left
            if imagex.status == STARTED and t >= frameRemains:
                imagex.setAutoDraw(False)
            
            # check for quit (typically the Esc key)
            if endExpNow or event.getKeys(keyList=["escape"]):
                core.quit()
            
            # check if all components have finished
            if not continueRoutine:  # a component has requested a forced-end of Routine
                break
            continueRoutine = False  # will revert to True if at least one component still running
            for thisComponent in trialComponents:
                if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                    continueRoutine = True
                    break  # at least one component has not yet finished
            
            # refresh the screen
            if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                win.flip()
        
        # -------Ending Routine "trial"-------
        for thisComponent in trialComponents:
            if hasattr(thisComponent, "setAutoDraw"):
                thisComponent.setAutoDraw(False)
        thisExp.nextEntry()
        
    # completed showImage repeats of 'should_image_be_shown'
    
    
    # set up handler to look after randomisation of conditions etc
    should_query_be_shown = data.TrialHandler(nReps=showQuery, method='sequential', 
        extraInfo=expInfo, originPath=-1,
        trialList=[None],
        seed=None, name='should_query_be_shown')
    thisExp.addLoop(should_query_be_shown)  # add the loop to the experiment
    thisShould_query_be_shown = should_query_be_shown.trialList[0]  # so we can initialise stimuli with some values
    # abbreviate parameter names if possible (e.g. rgb = thisShould_query_be_shown.rgb)
    if thisShould_query_be_shown != None:
        for paramName in thisShould_query_be_shown:
            exec('{} = thisShould_query_be_shown[paramName]'.format(paramName))
    
    for thisShould_query_be_shown in should_query_be_shown:
        currentLoop = should_query_be_shown
        # abbreviate parameter names if possible (e.g. rgb = thisShould_query_be_shown.rgb)
        if thisShould_query_be_shown != None:
            for paramName in thisShould_query_be_shown:
                exec('{} = thisShould_query_be_shown[paramName]'.format(paramName))
        
        # ------Prepare to start Routine "vigilance"-------
        t = 0
        vigilanceClock.reset()  # clock
        frameN = -1
        continueRoutine = True
        # update component parameters for each repeat
        image_query.setImage("stimuli_exp1/"+img_name)
        key_resp_4 = event.BuilderKeyResponse()
        # keep track of which components have finished
        vigilanceComponents = [image_query, key_resp_4, text_11, text_12]
        for thisComponent in vigilanceComponents:
            if hasattr(thisComponent, 'status'):
                thisComponent.status = NOT_STARTED
        
        # -------Start Routine "vigilance"-------
        while continueRoutine:
            # get current time
            t = vigilanceClock.getTime()
            frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
            # update/draw components on each frame
            
            # *image_query* updates
            if t >= 0.0 and image_query.status == NOT_STARTED:
                # keep track of start time/frame for later
                image_query.tStart = t
                image_query.frameNStart = frameN  # exact frame index
                image_query.setAutoDraw(True)
            
            # *key_resp_4* updates
            if t >= 0.0 and key_resp_4.status == NOT_STARTED:
                # keep track of start time/frame for later
                key_resp_4.tStart = t
                key_resp_4.frameNStart = frameN  # exact frame index
                key_resp_4.status = STARTED
                # keyboard checking is just starting
                win.callOnFlip(key_resp_4.clock.reset)  # t=0 on next screen flip
                event.clearEvents(eventType='keyboard')
            if key_resp_4.status == STARTED:
                theseKeys = event.getKeys(keyList=['left', 'right'])
                
                # check for quit:
                if "escape" in theseKeys:
                    endExpNow = True
                if len(theseKeys) > 0:  # at least one key was pressed
                    key_resp_4.keys = theseKeys[-1]  # just the last key pressed
                    key_resp_4.rt = key_resp_4.clock.getTime()
                    # was this 'correct'?
                    if (key_resp_4.keys == str(corrKey)) or (key_resp_4.keys == corrKey):
                        key_resp_4.corr = 1
                    else:
                        key_resp_4.corr = 0
                    # a response ends the routine
                    continueRoutine = False
            
            # *text_11* updates
            if t >= 0.0 and text_11.status == NOT_STARTED:
                # keep track of start time/frame for later
                text_11.tStart = t
                text_11.frameNStart = frameN  # exact frame index
                text_11.setAutoDraw(True)
            
            # *text_12* updates
            if t >= 0.0 and text_12.status == NOT_STARTED:
                # keep track of start time/frame for later
                text_12.tStart = t
                text_12.frameNStart = frameN  # exact frame index
                text_12.setAutoDraw(True)
            
            # check for quit (typically the Esc key)
            if endExpNow or event.getKeys(keyList=["escape"]):
                core.quit()
            
            # check if all components have finished
            if not continueRoutine:  # a component has requested a forced-end of Routine
                break
            continueRoutine = False  # will revert to True if at least one component still running
            for thisComponent in vigilanceComponents:
                if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                    continueRoutine = True
                    break  # at least one component has not yet finished
            
            # refresh the screen
            if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                win.flip()
        
        # -------Ending Routine "vigilance"-------
        for thisComponent in vigilanceComponents:
            if hasattr(thisComponent, "setAutoDraw"):
                thisComponent.setAutoDraw(False)
        # check responses
        if key_resp_4.keys in ['', [], None]:  # No response was made
            key_resp_4.keys=None
            # was no response the correct answer?!
            if str(corrKey).lower() == 'none':
               key_resp_4.corr = 1;  # correct non-response
            else:
               key_resp_4.corr = 0;  # failed to respond (incorrectly)
        # store data for should_query_be_shown (TrialHandler)
        should_query_be_shown.addData('key_resp_4.keys',key_resp_4.keys)
        should_query_be_shown.addData('key_resp_4.corr', key_resp_4.corr)
        if key_resp_4.keys != None:  # we had a response
            should_query_be_shown.addData('key_resp_4.rt', key_resp_4.rt)
        # the Routine "vigilance" was not non-slip safe, so reset the non-slip timer
        routineTimer.reset()
        
        # ------Prepare to start Routine "vigilance_rating"-------
        t = 0
        vigilance_ratingClock.reset()  # clock
        frameN = -1
        continueRoutine = True
        # update component parameters for each repeat
        image_query2.setImage("stimuli_exp1/"+img_name)
        rating.reset()
        # keep track of which components have finished
        vigilance_ratingComponents = [image_query2, rating]
        for thisComponent in vigilance_ratingComponents:
            if hasattr(thisComponent, 'status'):
                thisComponent.status = NOT_STARTED
        
        # -------Start Routine "vigilance_rating"-------
        while continueRoutine:
            # get current time
            t = vigilance_ratingClock.getTime()
            frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
            # update/draw components on each frame
            
            # *image_query2* updates
            if t >= 0.0 and image_query2.status == NOT_STARTED:
                # keep track of start time/frame for later
                image_query2.tStart = t
                image_query2.frameNStart = frameN  # exact frame index
                image_query2.setAutoDraw(True)
            # *rating* updates
            if t >= 0.0 and rating.status == NOT_STARTED:
                # keep track of start time/frame for later
                rating.tStart = t
                rating.frameNStart = frameN  # exact frame index
                rating.setAutoDraw(True)
            continueRoutine &= rating.noResponse  # a response ends the trial
            
            # check for quit (typically the Esc key)
            if endExpNow or event.getKeys(keyList=["escape"]):
                core.quit()
            
            # check if all components have finished
            if not continueRoutine:  # a component has requested a forced-end of Routine
                break
            continueRoutine = False  # will revert to True if at least one component still running
            for thisComponent in vigilance_ratingComponents:
                if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                    continueRoutine = True
                    break  # at least one component has not yet finished
            
            # refresh the screen
            if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                win.flip()
        
        # -------Ending Routine "vigilance_rating"-------
        for thisComponent in vigilance_ratingComponents:
            if hasattr(thisComponent, "setAutoDraw"):
                thisComponent.setAutoDraw(False)
        # store data for should_query_be_shown (TrialHandler)
        should_query_be_shown.addData('rating.response', rating.getRating())
        should_query_be_shown.addData('rating.rt', rating.getRT())
        # the Routine "vigilance_rating" was not non-slip safe, so reset the non-slip timer
        routineTimer.reset()
        thisExp.nextEntry()
        
    # completed showQuery repeats of 'should_query_be_shown'
    
    thisExp.nextEntry()
    
# completed 1 repeats of 'trialsA'


# ------Prepare to start Routine "thanks"-------
t = 0
thanksClock.reset()  # clock
frameN = -1
continueRoutine = True
routineTimer.add(3.000000)
# update component parameters for each repeat
text_4.setText('Děkujeme za účast')
# keep track of which components have finished
thanksComponents = [text_4]
for thisComponent in thanksComponents:
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED

# -------Start Routine "thanks"-------
while continueRoutine and routineTimer.getTime() > 0:
    # get current time
    t = thanksClock.getTime()
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *text_4* updates
    if t >= 0.0 and text_4.status == NOT_STARTED:
        # keep track of start time/frame for later
        text_4.tStart = t
        text_4.frameNStart = frameN  # exact frame index
        text_4.setAutoDraw(True)
    frameRemains = 0.0 + 3- win.monitorFramePeriod * 0.75  # most of one frame period left
    if text_4.status == STARTED and t >= frameRemains:
        text_4.setAutoDraw(False)
    
    # check for quit (typically the Esc key)
    if endExpNow or event.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in thanksComponents:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "thanks"-------
for thisComponent in thanksComponents:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
# these shouldn't be strictly necessary (should auto-save)
thisExp.saveAsWideText(filename+'.csv')
thisExp.saveAsPickle(filename)
logging.flush()
# make sure everything is closed down
thisExp.abort()  # or data files will save again on exit
win.close()
core.quit()
