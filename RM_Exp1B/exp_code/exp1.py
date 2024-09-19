#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
This experiment was created using PsychoPy3 Experiment Builder (v2023.1.0),
    on May 09, 2024, at 10:09
If you publish work using this script the most relevant publication is:

    Peirce J, Gray JR, Simpson S, MacAskill M, Höchenberger R, Sogo H, Kastman E, Lindeløv JK. (2019) 
        PsychoPy2: Experiments in behavior made easy Behav Res 51: 195. 
        https://doi.org/10.3758/s13428-018-01193-y

"""



# --- Import packages ---
from psychopy import locale_setup
from psychopy import prefs
from psychopy import plugins
plugins.activatePlugins()
prefs.hardware['audioLib'] = 'ptb'
prefs.hardware['audioLatencyMode'] = '3'
from psychopy import sound, gui, visual, core, data, event, logging, clock, colors, layout, monitors
from psychopy.constants import (NOT_STARTED, STARTED, PLAYING, PAUSED,
                                STOPPED, FINISHED, PRESSED, RELEASED, FOREVER)

import numpy as np  # whole numpy lib is available, prepend 'np.'
from numpy import (sin, cos, tan, log, log10, pi, average,
                   sqrt, std, deg2rad, rad2deg, linspace, asarray)
from numpy.random import random, randint, normal, shuffle, choice as randchoice
import os  # handy system and path functions
import sys  # to get file system encoding

import psychopy.iohub as io
from psychopy.hardware import keyboard

# Run 'Before Experiment' code from code_2
import random
import numpy as np
from psychopy.tools.monitorunittools import deg2pix

# Run 'Before Experiment' code from code
import random
import numpy as np
from psychopy.tools.monitorunittools import deg2pix
#deeekkkkle


# Ensure that relative paths start from the same directory as this script
_thisDir = os.path.dirname(os.path.abspath(__file__))
os.chdir(_thisDir)
# Store info about the experiment session
psychopyVersion = '2023.1.0'
expName = 'exp1'  # from the Builder filename that created this script
expInfo = {
    'participant': '',
    'age': '',
    'gender': ['female', 'male'],
    'resp_group': ['left_right_width', 'up_down_width'],
}
# --- Show participant info dialog --
dlg = gui.DlgFromDict(dictionary=expInfo, sortKeys=False, title=expName)
if dlg.OK == False:
    core.quit()  # user pressed cancel
expInfo['date'] = data.getDateStr()  # add a simple timestamp
expInfo['expName'] = expName
expInfo['psychopyVersion'] = psychopyVersion

# Data file name stem = absolute path + name; later add .psyexp, .csv, .log, etc
filename = _thisDir + os.sep + u'data/%s_%s_%s' % (expInfo['participant'], expName, expInfo['date'])

# An ExperimentHandler isn't essential but helps with data saving
thisExp = data.ExperimentHandler(name=expName, version='',
    extraInfo=expInfo, runtimeInfo=None,
    originPath='C:\\Users\\oztas\\Desktop\\exp1\\exp1.py',
    savePickle=True, saveWideText=True,
    dataFileName=filename)
# save a log file for detail verbose info
logFile = logging.LogFile(filename+'.log', level=logging.EXP)
logging.console.setLevel(logging.WARNING)  # this outputs to the screen, not a file

endExpNow = False  # flag for 'escape' or other condition => quit the exp
frameTolerance = 0.001  # how close to onset before 'same' frame

# Start Code - component code to be run after the window creation

monitor = 'default'  #monitor chosen
backgroundCol = [0, 0, 0] #background color
units = 'deg'
frameRateM = 60.0 # frame rate of the monitor
Resolution = [1920, 1200]
fullscrn = False
scr = 0 # screen that the stimulus are presented (only relevant if there are multiple monitors)
mondist = 57
monwidth = 33
Agui = False


# Setup the Window
monitorsetting = monitors.Monitor(monitor, width=monwidth, distance=mondist)
monitorsetting.setSizePix(Resolution)

# --- Setup the Window ---
win = visual.Window(
    size=Resolution, fullscr=fullscrn, screen=0, 
    winType='pyglet', allowStencil=False,
    monitor='default', color=[0,0,0], colorSpace='rgb',
    backgroundImage='', backgroundFit='none',
    blendMode='avg', useFBO=True, 
    units=units)
win.mouseVisible = False
# store frame rate of monitor if we can measure it
expInfo['frameRate'] = win.getActualFrameRate()
if expInfo['frameRate'] != None:
    frameDur = 1.0 / round(expInfo['frameRate'])
else:
    frameDur = 1.0 / 60.0  # could not measure, so guess
# --- Setup input devices ---
ioConfig = {}

# Setup iohub keyboard
ioConfig['Keyboard'] = dict(use_keymap='psychopy')

ioSession = '1'
if 'session' in expInfo:
    ioSession = str(expInfo['session'])
ioServer = io.launchHubServer(window=win, **ioConfig)
eyetracker = None

# create a default keyboard (e.g. to check for escape)
defaultKeyboard = keyboard.Keyboard(backend='iohub')

# --- Initialize components for Routine "intro" ---
yonerge_left_right = visual.TextStim(win=win, name='yonerge_left_right',
    text='',
    font='Times New Roman',
    units='deg', pos=(0, 0), height=1.0, wrapWidth=30.0, ori=0.0, 
    color='white', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=0.0);
yonerge_up_down = visual.TextStim(win=win, name='yonerge_up_down',
    text='',
    font='Times New Roman',
    units='deg', pos=(0, 0), height=1.0, wrapWidth=30.0, ori=0.0, 
    color='white', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=-1.0);
key_resp_3 = keyboard.Keyboard()

# --- Initialize components for Routine "interval" ---
fixation_interval = visual.GratingStim(
    win=win, name='fixation_interval',units='pix', 
    tex='sin', mask='circle', anchor='center',
    ori=0.0, pos=(0, 0), size=(8, 8), sf=40.0, phase=0.0,
    color=[-1,-1,-1], colorSpace='rgb',
    opacity=None, contrast=1.0, blendmode='avg',
    texRes=128.0, interpolate=True, depth=-1.0)

# --- Initialize components for Routine "stimuli_practice" ---
# Run 'Begin Experiment' code from code_2
h_p = 1.9

def trans_posi(posi):
    return -posi
p1_2 = visual.Rect(
    win=win, name='p1_2',units='deg', 
    width=[1.0, 1.0][0], height=[1.0, 1.0][1],
    ori=0.0, pos=[0,0], anchor='center',
    lineWidth=1.0,     colorSpace='rgb',  lineColor='black', fillColor='black',
    opacity=None, depth=-1.0, interpolate=True)
p2_2 = visual.Rect(
    win=win, name='p2_2',units='deg', 
    width=[1.0, 1.0][0], height=[1.0, 1.0][1],
    ori=0.0, pos=[0,0], anchor='center',
    lineWidth=1.0,     colorSpace='rgb',  lineColor='black', fillColor='black',
    opacity=None, depth=-2.0, interpolate=True)
p3_2 = visual.Rect(
    win=win, name='p3_2',units='deg', 
    width=[1.0, 1.0][0], height=[1.0, 1.0][1],
    ori=0.0, pos=[0,0], anchor='center',
    lineWidth=1.0,     colorSpace='rgb',  lineColor='black', fillColor='black',
    opacity=None, depth=-3.0, interpolate=True)
p4_2 = visual.Rect(
    win=win, name='p4_2',units='deg', 
    width=[1.0, 1.0][0], height=[1.0, 1.0][1],
    ori=0.0, pos=[0,0], anchor='center',
    lineWidth=1.0,     colorSpace='rgb',  lineColor='black', fillColor='black',
    opacity=None, depth=-4.0, interpolate=True)
p5_2 = visual.Rect(
    win=win, name='p5_2',units='deg', 
    width=[1.0, 1.0][0], height=[1.0, 1.0][1],
    ori=0.0, pos=[0,0], anchor='center',
    lineWidth=1.0,     colorSpace='rgb',  lineColor='black', fillColor='black',
    opacity=None, depth=-5.0, interpolate=True)
fixation_2 = visual.GratingStim(
    win=win, name='fixation_2',units='pix', 
    tex='sin', mask='circle', anchor='center',
    ori=0.0, pos=(0, 0), size=(8, 8), sf=40.0, phase=0.0,
    color=[-1,-1,-1], colorSpace='rgb',
    opacity=None, contrast=1.0, blendmode='avg',
    texRes=128.0, interpolate=True, depth=-6.0)

# --- Initialize components for Routine "response" ---
fixation_resp = visual.GratingStim(
    win=win, name='fixation_resp',units='pix', 
    tex='sin', mask='circle', anchor='center',
    ori=0.0, pos=(0, 0), size=(8, 8), sf=40.0, phase=0.0,
    color=[-1,-1,-1], colorSpace='rgb',
    opacity=None, contrast=1.0, blendmode='avg',
    texRes=128.0, interpolate=True, depth=0.0)
key_resp = keyboard.Keyboard()

# --- Initialize components for Routine "shaping" ---
# Run 'Begin Experiment' code from cd
renk_p1 = "black"
renk_p2 = "black"
renk_p3 = "black"
renk_p4 = "black"
renk_p5 = "black"
if expInfo["resp_group"] ==  "left_right_width":
    thinner = "left"
    thicker = "right"
    wider = "up"
    narrower ="down"
elif expInfo["resp_group"] == "up_down_width":
    thinner = "down"
    thicker = "up"
    wider = "right"
    narrower = "left"
test_p1_5 = visual.Rect(
    win=win, name='test_p1_5',units='deg', 
    width=[1.0, 1.0][0], height=[1.0, 1.0][1],
    ori=0.0, pos=[0,0], anchor='center',
    lineWidth=1.0,     colorSpace='rgb',  lineColor='white', fillColor='white',
    opacity=None, depth=-1.0, interpolate=True)
test_p2_5 = visual.Rect(
    win=win, name='test_p2_5',units='deg', 
    width=[1.0, 1.0][0], height=[1.0, 1.0][1],
    ori=0.0, pos=[0,0], anchor='center',
    lineWidth=1.0,     colorSpace='rgb',  lineColor='white', fillColor='white',
    opacity=None, depth=-2.0, interpolate=True)
test_p3_5 = visual.Rect(
    win=win, name='test_p3_5',units='deg', 
    width=[1.0, 1.0][0], height=[1.0, 1.0][1],
    ori=0.0, pos=[0,0], anchor='center',
    lineWidth=1.0,     colorSpace='rgb',  lineColor='white', fillColor='white',
    opacity=None, depth=-3.0, interpolate=True)
test_p4_5 = visual.Rect(
    win=win, name='test_p4_5',units='deg', 
    width=[1.0, 1.0][0], height=[1.0, 1.0][1],
    ori=0.0, pos=[0,0], anchor='center',
    lineWidth=1.0,     colorSpace='rgb',  lineColor='white', fillColor='white',
    opacity=None, depth=-4.0, interpolate=True)
test_p5_5 = visual.Rect(
    win=win, name='test_p5_5',units='deg', 
    width=[1.0, 1.0][0], height=[1.0, 1.0][1],
    ori=0.0, pos=[0,0], anchor='center',
    lineWidth=1.0,     colorSpace='rgb',  lineColor='white', fillColor='white',
    opacity=None, depth=-5.0, interpolate=True)
test_p6 = visual.Rect(
    win=win, name='test_p6',units='deg', 
    width=[1.0, 1.0][0], height=[1.0, 1.0][1],
    ori=0.0, pos=[0,0], anchor='center',
    lineWidth=1.0,     colorSpace='rgb',  lineColor='white', fillColor='white',
    opacity=None, depth=-6.0, interpolate=True)
test_p7 = visual.Rect(
    win=win, name='test_p7',units='deg', 
    width=[1.0, 1.0][0], height=[1.0, 1.0][1],
    ori=0.0, pos=[0,0], anchor='center',
    lineWidth=1.0,     colorSpace='rgb',  lineColor='white', fillColor='white',
    opacity=None, depth=-7.0, interpolate=True)
test_p8 = visual.Rect(
    win=win, name='test_p8',units='deg', 
    width=[1.0, 1.0][0], height=[1.0, 1.0][1],
    ori=0.0, pos=[0,0], anchor='center',
    lineWidth=1.0,     colorSpace='rgb',  lineColor='white', fillColor='white',
    opacity=None, depth=-8.0, interpolate=True)
test_p9 = visual.Rect(
    win=win, name='test_p9',units='deg', 
    width=[1.0, 1.0][0], height=[1.0, 1.0][1],
    ori=0.0, pos=[0,0], anchor='center',
    lineWidth=1.0,     colorSpace='rgb',  lineColor='white', fillColor='white',
    opacity=None, depth=-9.0, interpolate=True)
next_5 = keyboard.Keyboard()

# --- Initialize components for Routine "intro_2" ---
text = visual.TextStim(win=win, name='text',
    text="The training trials are over. Before the actual experiment begins, here are a few reminders:\n\n1)\n\nAt the start of each trial, focus only on the central dot. Do not look at the lines that appear on the sides.\n\n\n\n2)The number of lines, their thickness, and the distance between them will vary randomly in each trial.\n\n\n\n3)This study consists of 6 blocks, and you will have a rest period after each block. The study typically takes around 40 minutes.\n\n\n\nIf you have any questions or concerns about the controls or the study, you can ask the experimenter before starting. You can begin by pressing the 'c' key.",
    font='Times New Roman',
    units='deg', pos=(0, 0), height=1.0, wrapWidth=30.0, ori=0.0, 
    color='white', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=0.0);
key_resp_4 = keyboard.Keyboard()

# --- Initialize components for Routine "interval" ---
fixation_interval = visual.GratingStim(
    win=win, name='fixation_interval',units='pix', 
    tex='sin', mask='circle', anchor='center',
    ori=0.0, pos=(0, 0), size=(8, 8), sf=40.0, phase=0.0,
    color=[-1,-1,-1], colorSpace='rgb',
    opacity=None, contrast=1.0, blendmode='avg',
    texRes=128.0, interpolate=True, depth=-1.0)
# --- Initialize components for Routine "info" ---
text_2 = visual.TextStim(win=win, name='text_2',
    text='This study has the same controls as before and consists of two stages.\n\n In the first stage, a line will be shown to you on the right or left side of the screen, and then you will be asked to adjust the thickness of that line. You can adjust the thickness of the line with the left-right direction keys. Once you have made your adjustments, you can proceed to the next trial by pressing the "enter" key.\n\nYou can start by pressing the "c" key.',
    font='Times New Roman',
    units='deg', pos=(0, 0), height=1.0, wrapWidth=30.0, ori=0.0, 
    color='white', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=0.0);
key_resp_2 = keyboard.Keyboard()

# --- Initialize components for Routine "wait" ---
fixation_4 = visual.GratingStim(
    win=win, name='fixation_4',units='pix', 
    tex='sin', mask='circle', anchor='center',
    ori=0.0, pos=(0, 0), size=(8, 8), sf=40.0, phase=0.0,
    color=[-1,-1,-1], colorSpace='rgb',
    opacity=None, contrast=1.0, blendmode='avg',
    texRes=128.0, interpolate=True, depth=-1.0)

# --- Initialize components for Routine "present" ---
# Run 'Begin Experiment' code from code_2
def trans_posi(posi):
    return -posi

stimuli_2 = visual.Rect(
    win=win, name='stimuli_2',units='deg', 
    width=[1.0, 1.0][0], height=[1.0, 1.0][1],
    ori=0.0, pos=[0,0], anchor='center',
    lineWidth=1.0,     colorSpace='rgb',  lineColor='black', fillColor='black',
    opacity=None, depth=-1.0, interpolate=True)
fixation = visual.GratingStim(
    win=win, name='fixation',units='pix', 
    tex='sin', mask='circle', anchor='center',
    ori=0.0, pos=(0, 0), size=(8, 8), sf=40.0, phase=0.0,
    color=[-1,-1,-1], colorSpace='rgb',
    opacity=None, contrast=1.0, blendmode='avg',
    texRes=128.0, interpolate=True, depth=-2.0)

# --- Initialize components for Routine "resp" ---
# Run 'Begin Experiment' code from code


adjusting = visual.Rect(
    win=win, name='adjusting',units='deg', 
    width=[1.0, 1.0][0], height=[1.0, 1.0][1],
    ori=0.0, pos=(0, 0), anchor='center',
    lineWidth=1.0,     colorSpace='rgb',  lineColor='black', fillColor='black',
    opacity=None, depth=-1.0, interpolate=True)
next = keyboard.Keyboard()

# --- Initialize components for Routine "info_2" ---
text_control2 = visual.TextStim(win=win, name='text_control2',
    text='The first stage is completed. \n\nIn the second stage, either one line or two lines will be shown to you on the right or left side of the screen. Then, you need to indicate how many lines there are by pressing the "1" or "2" keys. Once you have responded, the next trial will begin.\n\nYou can start by pressing the "c" key.\n\n',
    font='Times New Roman',
    units='deg', pos=(0, 0), height=1.0, wrapWidth=30.0, ori=0.0, 
    color='white', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=0.0);
key_resp_3 = keyboard.Keyboard()

# --- Initialize components for Routine "wait" ---
fixation_4 = visual.GratingStim(
    win=win, name='fixation_4',units='pix', 
    tex='sin', mask='circle', anchor='center',
    ori=0.0, pos=(0, 0), size=(8, 8), sf=40.0, phase=0.0,
    color=[-1,-1,-1], colorSpace='rgb',
    opacity=None, contrast=1.0, blendmode='avg',
    texRes=128.0, interpolate=True, depth=-1.0)

# --- Initialize components for Routine "present_2" ---
# Run 'Begin Experiment' code from code_3
def trans_posi(posi):
    return -posi

stimuli1 = visual.Rect(
    win=win, name='stimuli1',units='deg', 
    width=[1.0, 1.0][0], height=[1.0, 1.0][1],
    ori=0.0, pos=[0,0], anchor='center',
    lineWidth=1.0,     colorSpace='rgb',  lineColor='black', fillColor='black',
    opacity=None, depth=-1.0, interpolate=True)
stimuli2 = visual.Rect(
    win=win, name='stimuli2',units='deg', 
    width=[1.0, 1.0][0], height=[1.0, 1.0][1],
    ori=0.0, pos=[0,0], anchor='center',
    lineWidth=1.0,     colorSpace='rgb',  lineColor='black', fillColor='black',
    opacity=None, depth=-2.0, interpolate=True)
fixation_2 = visual.GratingStim(
    win=win, name='fixation_2',units='pix', 
    tex='sin', mask='circle', anchor='center',
    ori=0.0, pos=(0, 0), size=(8, 8), sf=40.0, phase=0.0,
    color=[-1,-1,-1], colorSpace='rgb',
    opacity=None, contrast=1.0, blendmode='avg',
    texRes=128.0, interpolate=True, depth=-3.0)

# --- Initialize components for Routine "resp_2" ---
fixation_3 = visual.GratingStim(
    win=win, name='fixation_3',units='pix', 
    tex='sin', mask='circle', anchor='center',
    ori=0.0, pos=(0, 0), size=(8, 8), sf=40.0, phase=0.0,
    color=[-1,-1,-1], colorSpace='rgb',
    opacity=None, contrast=1.0, blendmode='avg',
    texRes=128.0, interpolate=True, depth=0.0)
key_resp = keyboard.Keyboard()

# --- Initialize components for Routine "bye" ---
text_bye = visual.TextStim(win=win, name='text',
    text='Thanks for the participation',
    font='Times New Roman',
    pos=(0, 0), height=1, wrapWidth=None, ori=0.0, 
    color='white', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=0.0);
# --- Initialize components for Routine "stimuli" ---
# Run 'Begin Experiment' code from code
h = 1.9

def trans_posi(posi):
    return -posi
p1 = visual.Rect(
    win=win, name='p1',units='deg', 
    width=[1.0, 1.0][0], height=[1.0, 1.0][1],
    ori=0.0, pos=[0,0], anchor='center',
    lineWidth=1.0,     colorSpace='rgb',  lineColor='black', fillColor='black',
    opacity=None, depth=-1.0, interpolate=True)
p2 = visual.Rect(
    win=win, name='p2',units='deg', 
    width=[1.0, 1.0][0], height=[1.0, 1.0][1],
    ori=0.0, pos=[0,0], anchor='center',
    lineWidth=1.0,     colorSpace='rgb',  lineColor='black', fillColor='black',
    opacity=None, depth=-2.0, interpolate=True)
p3 = visual.Rect(
    win=win, name='p3',units='deg', 
    width=[1.0, 1.0][0], height=[1.0, 1.0][1],
    ori=0.0, pos=[0,0], anchor='center',
    lineWidth=1.0,     colorSpace='rgb',  lineColor='black', fillColor='black',
    opacity=None, depth=-3.0, interpolate=True)
p4 = visual.Rect(
    win=win, name='p4',units='deg', 
    width=[1.0, 1.0][0], height=[1.0, 1.0][1],
    ori=0.0, pos=[0,0], anchor='center',
    lineWidth=1.0,     colorSpace='rgb',  lineColor='black', fillColor='black',
    opacity=None, depth=-4.0, interpolate=True)
p5 = visual.Rect(
    win=win, name='p5',units='deg', 
    width=[1.0, 1.0][0], height=[1.0, 1.0][1],
    ori=0.0, pos=[0,0], anchor='center',
    lineWidth=1.0,     colorSpace='rgb',  lineColor='black', fillColor='black',
    opacity=None, depth=-5.0, interpolate=True)
fixation = visual.GratingStim(
    win=win, name='fixation',units='pix', 
    tex='sin', mask='circle', anchor='center',
    ori=0.0, pos=(0, 0), size=(8, 8), sf=40.0, phase=0.0,
    color=[-1,-1,-1], colorSpace='rgb',
    opacity=None, contrast=1.0, blendmode='avg',
    texRes=128.0, interpolate=True, depth=-6.0)

# --- Initialize components for Routine "response" ---
fixation_resp = visual.GratingStim(
    win=win, name='fixation_resp',units='pix', 
    tex='sin', mask='circle', anchor='center',
    ori=0.0, pos=(0, 0), size=(8, 8), sf=40.0, phase=0.0,
    color=[-1,-1,-1], colorSpace='rgb',
    opacity=None, contrast=1.0, blendmode='avg',
    texRes=128.0, interpolate=True, depth=0.0)
key_resp = keyboard.Keyboard()

# --- Initialize components for Routine "shaping" ---
# Run 'Begin Experiment' code from cd
renk_p1 = "black"
renk_p2 = "black"
renk_p3 = "black"
renk_p4 = "black"
renk_p5 = "black"
if expInfo["resp_group"] ==  "left_right_width":
    thinner = "left"
    thicker = "right"
    wider = "up"
    narrower ="down"
elif expInfo["resp_group"] == "up_down_width":
    thinner = "down"
    thicker = "up"
    wider = "right"
    narrower = "left"
test_p1_5 = visual.Rect(
    win=win, name='test_p1_5',units='deg', 
    width=[1.0, 1.0][0], height=[1.0, 1.0][1],
    ori=0.0, pos=[0,0], anchor='center',
    lineWidth=1.0,     colorSpace='rgb',  lineColor='white', fillColor='white',
    opacity=None, depth=-1.0, interpolate=True)
test_p2_5 = visual.Rect(
    win=win, name='test_p2_5',units='deg', 
    width=[1.0, 1.0][0], height=[1.0, 1.0][1],
    ori=0.0, pos=[0,0], anchor='center',
    lineWidth=1.0,     colorSpace='rgb',  lineColor='white', fillColor='white',
    opacity=None, depth=-2.0, interpolate=True)
test_p3_5 = visual.Rect(
    win=win, name='test_p3_5',units='deg', 
    width=[1.0, 1.0][0], height=[1.0, 1.0][1],
    ori=0.0, pos=[0,0], anchor='center',
    lineWidth=1.0,     colorSpace='rgb',  lineColor='white', fillColor='white',
    opacity=None, depth=-3.0, interpolate=True)
test_p4_5 = visual.Rect(
    win=win, name='test_p4_5',units='deg', 
    width=[1.0, 1.0][0], height=[1.0, 1.0][1],
    ori=0.0, pos=[0,0], anchor='center',
    lineWidth=1.0,     colorSpace='rgb',  lineColor='white', fillColor='white',
    opacity=None, depth=-4.0, interpolate=True)
test_p5_5 = visual.Rect(
    win=win, name='test_p5_5',units='deg', 
    width=[1.0, 1.0][0], height=[1.0, 1.0][1],
    ori=0.0, pos=[0,0], anchor='center',
    lineWidth=1.0,     colorSpace='rgb',  lineColor='white', fillColor='white',
    opacity=None, depth=-5.0, interpolate=True)
test_p6 = visual.Rect(
    win=win, name='test_p6',units='deg', 
    width=[1.0, 1.0][0], height=[1.0, 1.0][1],
    ori=0.0, pos=[0,0], anchor='center',
    lineWidth=1.0,     colorSpace='rgb',  lineColor='white', fillColor='white',
    opacity=None, depth=-6.0, interpolate=True)
test_p7 = visual.Rect(
    win=win, name='test_p7',units='deg', 
    width=[1.0, 1.0][0], height=[1.0, 1.0][1],
    ori=0.0, pos=[0,0], anchor='center',
    lineWidth=1.0,     colorSpace='rgb',  lineColor='white', fillColor='white',
    opacity=None, depth=-7.0, interpolate=True)
test_p8 = visual.Rect(
    win=win, name='test_p8',units='deg', 
    width=[1.0, 1.0][0], height=[1.0, 1.0][1],
    ori=0.0, pos=[0,0], anchor='center',
    lineWidth=1.0,     colorSpace='rgb',  lineColor='white', fillColor='white',
    opacity=None, depth=-8.0, interpolate=True)
test_p9 = visual.Rect(
    win=win, name='test_p9',units='deg', 
    width=[1.0, 1.0][0], height=[1.0, 1.0][1],
    ori=0.0, pos=[0,0], anchor='center',
    lineWidth=1.0,     colorSpace='rgb',  lineColor='white', fillColor='white',
    opacity=None, depth=-9.0, interpolate=True)
next_5 = keyboard.Keyboard()

# --- Initialize components for Routine "break_2" ---
text_3 = visual.TextStim(win=win, name='text_3',
    text="You can take a break if needed now.\n\n\n\nPress 'c' to continue.",
    font='Open Sans',
    units='height', pos=(0, 0.02), height=0.025, wrapWidth=None, ori=0.0, 
    color='white', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=0.0);
text_4 = visual.TextStim(win=win, name='text_4',
    text='',
    font='Open Sans',
    units='height', pos=(0, -0.1), height=0.025, wrapWidth=None, ori=0.0, 
    color='white', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=-1.0);
key_resp_2 = keyboard.Keyboard()


# Create some handy timers
globalClock = core.Clock()  # to track the time since experiment started
routineTimer = core.Clock()  # to track time remaining of each (possibly non-slip) routine 

# --- Prepare to start Routine "intro" ---
continueRoutine = True
# update component parameters for each repeat
yonerge_left_right.setText("When the experiment starts, you will see a black dot in the center. You should not take your eyes off this dot. Next, on the left or right side of the screen, a few lines will briefly appear. What is required of you is to keep your eyes on the center dot and try to perceive the number of lines, their thickness, and the distance between them that falls within your field of vision. You need to respond with the number of lines you perceive using numbers from 1 to 9. After that, you will be shown lines with random distances and random thickness, and you are expected to adjust these variables to match what you perceived. You can use the left and right arrow keys to make the lines thinner or thicker. To increase the distance between the lines, use the up arrow key, and to bring them closer, use the down arrow key. When you believe the lines match the ones you perceived, press the Enter key to proceed to the next trial.\n\n\n\nBefore the experiment begins, there will be a few practice runs to help you learn. You can start by pressing the 'c' key.")
yonerge_up_down.setText("When the experiment starts, you will see a black dot in the center. It's essential not to take your eyes off this dot. Afterward, on the left or right side of the screen, a few lines will briefly appear. What is required of you is to keep your eyes on the center dot and attempt to perceive the number of lines, their thickness, and the distance between them that falls within your field of vision. You need to respond using numbers from 1 to 9 to indicate how many lines you perceive. After that, you will be shown lines with random distances and random thickness, and you are expected to adjust these variables to match what you perceived. You can make the lines thinner or thicker using the up and down arrow keys. To increase the distance between the lines, use the right arrow key, and to bring them closer, use the left arrow key. When you believe the lines match the ones you perceived, press the Enter key to move on to the next trial.\n\n\n\nBefore the experiment begins, there will be a few practice runs to help you learn. You can start by pressing the 'c' key.")
key_resp_3.keys = []
key_resp_3.rt = []
_key_resp_3_allKeys = []
# keep track of which components have finished
introComponents = [yonerge_left_right, yonerge_up_down, key_resp_3]
for thisComponent in introComponents:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
frameN = -1

# --- Run Routine "intro" ---
routineForceEnded = not continueRoutine
while continueRoutine:
    # get current time
    t = routineTimer.getTime()
    tThisFlip = win.getFutureFlipTime(clock=routineTimer)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *yonerge_left_right* updates
    
    # if yonerge_left_right is starting this frame...
    if yonerge_left_right.status == NOT_STARTED and expInfo["resp_group"] ==  "left_right_width":
        # keep track of start time/frame for later
        yonerge_left_right.frameNStart = frameN  # exact frame index
        yonerge_left_right.tStart = t  # local t and not account for scr refresh
        yonerge_left_right.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(yonerge_left_right, 'tStartRefresh')  # time at next scr refresh
        # update status
        yonerge_left_right.status = STARTED
        yonerge_left_right.setAutoDraw(True)
    
    # if yonerge_left_right is active this frame...
    if yonerge_left_right.status == STARTED:
        # update params
        pass
    
    # *yonerge_up_down* updates
    
    # if yonerge_up_down is starting this frame...
    if yonerge_up_down.status == NOT_STARTED and expInfo["resp_group"] ==  "up_down_width":
        # keep track of start time/frame for later
        yonerge_up_down.frameNStart = frameN  # exact frame index
        yonerge_up_down.tStart = t  # local t and not account for scr refresh
        yonerge_up_down.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(yonerge_up_down, 'tStartRefresh')  # time at next scr refresh
        # update status
        yonerge_up_down.status = STARTED
        yonerge_up_down.setAutoDraw(True)
    
    # if yonerge_up_down is active this frame...
    if yonerge_up_down.status == STARTED:
        # update params
        pass
    
    # *key_resp_3* updates
    waitOnFlip = False
    
    # if key_resp_3 is starting this frame...
    if key_resp_3.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        key_resp_3.frameNStart = frameN  # exact frame index
        key_resp_3.tStart = t  # local t and not account for scr refresh
        key_resp_3.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(key_resp_3, 'tStartRefresh')  # time at next scr refresh
        # add timestamp to datafile
        thisExp.timestampOnFlip(win, 'key_resp_3.started')
        # update status
        key_resp_3.status = STARTED
        # keyboard checking is just starting
        waitOnFlip = True
        win.callOnFlip(key_resp_3.clock.reset)  # t=0 on next screen flip
        win.callOnFlip(key_resp_3.clearEvents, eventType='keyboard')  # clear events on next screen flip
    if key_resp_3.status == STARTED and not waitOnFlip:
        theseKeys = key_resp_3.getKeys(keyList=["c"], waitRelease=True)
        _key_resp_3_allKeys.extend(theseKeys)
        if len(_key_resp_3_allKeys):
            key_resp_3.keys = _key_resp_3_allKeys[-1].name  # just the last key pressed
            key_resp_3.rt = _key_resp_3_allKeys[-1].rt
            # a response ends the routine
            continueRoutine = False
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        routineForceEnded = True
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in introComponents:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# --- Ending Routine "intro" ---
for thisComponent in introComponents:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
# check responses
if key_resp_3.keys in ['', [], None]:  # No response was made
    key_resp_3.keys = None
thisExp.addData('key_resp_3.keys',key_resp_3.keys)
if key_resp_3.keys != None:  # we had a response
    thisExp.addData('key_resp_3.rt', key_resp_3.rt)
thisExp.nextEntry()
# the Routine "intro" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# set up handler to look after randomisation of conditions etc
trials = data.TrialHandler(nReps=1, method='random', 
    extraInfo=expInfo, originPath=-1,
    trialList=data.importConditions('practice_trials.xlsx'),
    seed=None, name='trials')
thisExp.addLoop(trials)  # add the loop to the experiment
thisTrial = trials.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb = thisTrial.rgb)
if thisTrial != None:
    for paramName in thisTrial:
        exec('{} = thisTrial[paramName]'.format(paramName))

for thisTrial in trials:
    currentLoop = trials
    # abbreviate parameter names if possible (e.g. rgb = thisTrial.rgb)
    if thisTrial != None:
        for paramName in thisTrial:
            exec('{} = thisTrial[paramName]'.format(paramName))
    
    # --- Prepare to start Routine "interval" ---
    continueRoutine = True
    # update component parameters for each repeat
    # Run 'Begin Routine' code from code_5
    random_n = [1,1.25,1.5,1.75,2]
    shuffle(random_n)
    # keep track of which components have finished
    intervalComponents = [fixation_interval]
    for thisComponent in intervalComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    frameN = -1
    
    # --- Run Routine "interval" ---
    routineForceEnded = not continueRoutine
    while continueRoutine:
        # get current time
        t = routineTimer.getTime()
        tThisFlip = win.getFutureFlipTime(clock=routineTimer)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *fixation_interval* updates
        
        # if fixation_interval is starting this frame...
        if fixation_interval.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            fixation_interval.frameNStart = frameN  # exact frame index
            fixation_interval.tStart = t  # local t and not account for scr refresh
            fixation_interval.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(fixation_interval, 'tStartRefresh')  # time at next scr refresh
            # update status
            fixation_interval.status = STARTED
            fixation_interval.setAutoDraw(True)
        
        # if fixation_interval is active this frame...
        if fixation_interval.status == STARTED:
            # update params
            pass
        
        # if fixation_interval is stopping this frame...
        if fixation_interval.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > fixation_interval.tStartRefresh + random_n[0]-frameTolerance:
                # keep track of stop time/frame for later
                fixation_interval.tStop = t  # not accounting for scr refresh
                fixation_interval.frameNStop = frameN  # exact frame index
                # update status
                fixation_interval.status = FINISHED
                fixation_interval.setAutoDraw(False)
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            routineForceEnded = True
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in intervalComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # --- Ending Routine "interval" ---
    for thisComponent in intervalComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # the Routine "interval" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # --- Prepare to start Routine "stimuli_practice" ---
    continueRoutine = True
    # update component parameters for each repeat
    # Run 'Begin Routine' code from code_2
    x = random.random()
    
    if x < 0.5:
        posx1 = trans_posi(posx1_p)
        posx2 = trans_posi(posx2_p)
        posx3 = trans_posi(posx3_p)
        posx4 = trans_posi(posx4_p)
        posx5 = trans_posi(posx5_p)
    p1_2.setPos((posx1_p, 0))
    p1_2.setSize((w_p, h_p))
    p2_2.setPos((posx2_p, 0))
    p2_2.setSize((w_p, h_p))
    p3_2.setPos((posx3_p, 0))
    p3_2.setSize((w_p, h_p))
    p4_2.setPos((posx4_p, 0))
    p4_2.setSize((w_p, h_p))
    p5_2.setPos((posx5_p, 0))
    p5_2.setSize((w_p, h_p))
    # keep track of which components have finished
    stimuli_practiceComponents = [p1_2, p2_2, p3_2, p4_2, p5_2, fixation_2]
    for thisComponent in stimuli_practiceComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    frameN = -1
    
    # --- Run Routine "stimuli_practice" ---
    routineForceEnded = not continueRoutine
    while continueRoutine:
        # get current time
        t = routineTimer.getTime()
        tThisFlip = win.getFutureFlipTime(clock=routineTimer)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *p1_2* updates
        
        # if p1_2 is starting this frame...
        if p1_2.status == NOT_STARTED and tThisFlip >= 0-frameTolerance:
            # keep track of start time/frame for later
            p1_2.frameNStart = frameN  # exact frame index
            p1_2.tStart = t  # local t and not account for scr refresh
            p1_2.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(p1_2, 'tStartRefresh')  # time at next scr refresh
            # update status
            p1_2.status = STARTED
            p1_2.setAutoDraw(True)
        
        # if p1_2 is active this frame...
        if p1_2.status == STARTED:
            # update params
            pass
        
        # if p1_2 is stopping this frame...
        if p1_2.status == STARTED:
            if frameN >= (p1_2.frameNStart + line1_p):
                # keep track of stop time/frame for later
                p1_2.tStop = t  # not accounting for scr refresh
                p1_2.frameNStop = frameN  # exact frame index
                # update status
                p1_2.status = FINISHED
                p1_2.setAutoDraw(False)
        
        # *p2_2* updates
        
        # if p2_2 is starting this frame...
        if p2_2.status == NOT_STARTED and tThisFlip >= 0-frameTolerance:
            # keep track of start time/frame for later
            p2_2.frameNStart = frameN  # exact frame index
            p2_2.tStart = t  # local t and not account for scr refresh
            p2_2.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(p2_2, 'tStartRefresh')  # time at next scr refresh
            # update status
            p2_2.status = STARTED
            p2_2.setAutoDraw(True)
        
        # if p2_2 is active this frame...
        if p2_2.status == STARTED:
            # update params
            pass
        
        # if p2_2 is stopping this frame...
        if p2_2.status == STARTED:
            if frameN >= (p2_2.frameNStart + line2_p):
                # keep track of stop time/frame for later
                p2_2.tStop = t  # not accounting for scr refresh
                p2_2.frameNStop = frameN  # exact frame index
                # update status
                p2_2.status = FINISHED
                p2_2.setAutoDraw(False)
        
        # *p3_2* updates
        
        # if p3_2 is starting this frame...
        if p3_2.status == NOT_STARTED and tThisFlip >= 0-frameTolerance:
            # keep track of start time/frame for later
            p3_2.frameNStart = frameN  # exact frame index
            p3_2.tStart = t  # local t and not account for scr refresh
            p3_2.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(p3_2, 'tStartRefresh')  # time at next scr refresh
            # update status
            p3_2.status = STARTED
            p3_2.setAutoDraw(True)
        
        # if p3_2 is active this frame...
        if p3_2.status == STARTED:
            # update params
            pass
        
        # if p3_2 is stopping this frame...
        if p3_2.status == STARTED:
            if frameN >= (p3_2.frameNStart + line3_p):
                # keep track of stop time/frame for later
                p3_2.tStop = t  # not accounting for scr refresh
                p3_2.frameNStop = frameN  # exact frame index
                # update status
                p3_2.status = FINISHED
                p3_2.setAutoDraw(False)
        
        # *p4_2* updates
        
        # if p4_2 is starting this frame...
        if p4_2.status == NOT_STARTED and tThisFlip >= 0-frameTolerance:
            # keep track of start time/frame for later
            p4_2.frameNStart = frameN  # exact frame index
            p4_2.tStart = t  # local t and not account for scr refresh
            p4_2.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(p4_2, 'tStartRefresh')  # time at next scr refresh
            # update status
            p4_2.status = STARTED
            p4_2.setAutoDraw(True)
        
        # if p4_2 is active this frame...
        if p4_2.status == STARTED:
            # update params
            pass
        
        # if p4_2 is stopping this frame...
        if p4_2.status == STARTED:
            if frameN >= (p4_2.frameNStart + line4_p):
                # keep track of stop time/frame for later
                p4_2.tStop = t  # not accounting for scr refresh
                p4_2.frameNStop = frameN  # exact frame index
                # update status
                p4_2.status = FINISHED
                p4_2.setAutoDraw(False)
        
        # *p5_2* updates
        
        # if p5_2 is starting this frame...
        if p5_2.status == NOT_STARTED and tThisFlip >= 0-frameTolerance:
            # keep track of start time/frame for later
            p5_2.frameNStart = frameN  # exact frame index
            p5_2.tStart = t  # local t and not account for scr refresh
            p5_2.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(p5_2, 'tStartRefresh')  # time at next scr refresh
            # update status
            p5_2.status = STARTED
            p5_2.setAutoDraw(True)
        
        # if p5_2 is active this frame...
        if p5_2.status == STARTED:
            # update params
            pass
        
        # if p5_2 is stopping this frame...
        if p5_2.status == STARTED:
            if frameN >= (p5_2.frameNStart + line5_p):
                # keep track of stop time/frame for later
                p5_2.tStop = t  # not accounting for scr refresh
                p5_2.frameNStop = frameN  # exact frame index
                # update status
                p5_2.status = FINISHED
                p5_2.setAutoDraw(False)
        
        # *fixation_2* updates
        
        # if fixation_2 is starting this frame...
        if fixation_2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            fixation_2.frameNStart = frameN  # exact frame index
            fixation_2.tStart = t  # local t and not account for scr refresh
            fixation_2.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(fixation_2, 'tStartRefresh')  # time at next scr refresh
            # update status
            fixation_2.status = STARTED
            fixation_2.setAutoDraw(True)
        
        # if fixation_2 is active this frame...
        if fixation_2.status == STARTED:
            # update params
            pass
        
        # if fixation_2 is stopping this frame...
        if fixation_2.status == STARTED:
            if frameN >= (fixation_2.frameNStart + 9):
                # keep track of stop time/frame for later
                fixation_2.tStop = t  # not accounting for scr refresh
                fixation_2.frameNStop = frameN  # exact frame index
                # update status
                fixation_2.status = FINISHED
                fixation_2.setAutoDraw(False)
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            routineForceEnded = True
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in stimuli_practiceComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # --- Ending Routine "stimuli_practice" ---
    for thisComponent in stimuli_practiceComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # the Routine "stimuli_practice" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # --- Prepare to start Routine "response" ---
    continueRoutine = True
    # update component parameters for each repeat
    key_resp.keys = []
    key_resp.rt = []
    _key_resp_allKeys = []
    # keep track of which components have finished
    responseComponents = [fixation_resp, key_resp]
    for thisComponent in responseComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    frameN = -1
    
    # --- Run Routine "response" ---
    routineForceEnded = not continueRoutine
    while continueRoutine:
        # get current time
        t = routineTimer.getTime()
        tThisFlip = win.getFutureFlipTime(clock=routineTimer)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *fixation_resp* updates
        
        # if fixation_resp is starting this frame...
        if fixation_resp.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            fixation_resp.frameNStart = frameN  # exact frame index
            fixation_resp.tStart = t  # local t and not account for scr refresh
            fixation_resp.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(fixation_resp, 'tStartRefresh')  # time at next scr refresh
            # update status
            fixation_resp.status = STARTED
            fixation_resp.setAutoDraw(True)
        
        # if fixation_resp is active this frame...
        if fixation_resp.status == STARTED:
            # update params
            pass
        
        # *key_resp* updates
        waitOnFlip = False
        
        # if key_resp is starting this frame...
        if key_resp.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            key_resp.frameNStart = frameN  # exact frame index
            key_resp.tStart = t  # local t and not account for scr refresh
            key_resp.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(key_resp, 'tStartRefresh')  # time at next scr refresh
            # update status
            key_resp.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(key_resp.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(key_resp.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if key_resp.status == STARTED and not waitOnFlip:
            theseKeys = key_resp.getKeys(keyList=["num_1","num_2","num_3","num_4","num_5","num_6","num_7","num_8","num_9"], waitRelease=False)
            _key_resp_allKeys.extend(theseKeys)
            if len(_key_resp_allKeys):
                key_resp.keys = _key_resp_allKeys[-1].name  # just the last key pressed
                key_resp.rt = _key_resp_allKeys[-1].rt
                # a response ends the routine
                continueRoutine = False
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            routineForceEnded = True
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in responseComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # --- Ending Routine "response" ---
    for thisComponent in responseComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # check responses
    if key_resp.keys in ['', [], None]:  # No response was made
        key_resp.keys = None
    trials.addData('key_resp.keys',key_resp.keys)
    if key_resp.keys != None:  # we had a response
        trials.addData('key_resp.rt', key_resp.rt)
    # Run 'End Routine' code from response_code
    thisExp.addData("response",key_resp.keys)
    #to determine the how many lines should be presented after this response
    if key_resp.keys == "num_1":
        shaping_1 = True
        shaping_2 = False
        shaping_3 = False
        shaping_4 = False
        shaping_5 = False
        shaping_6 = False
        shaping_7 = False
        shaping_8 = False
        shaping_9 = False
    elif key_resp.keys == "num_2":
        shaping_1 = False
        shaping_2 = True
        shaping_3 = False
        shaping_4 = False
        shaping_5 = False
        shaping_6 = False
        shaping_7 = False
        shaping_8 = False
        shaping_9 = False
    elif key_resp.keys == "num_3":
        shaping_1 = False
        shaping_2 = False
        shaping_3 = True
        shaping_4 = False
        shaping_5 = False
        shaping_6 = False
        shaping_7 = False
        shaping_8 = False
        shaping_9 = False
    elif key_resp.keys == "num_4":
        shaping_1 = False
        shaping_2 = False
        shaping_3 = False
        shaping_4 = True
        shaping_5 = False
        shaping_6 = False
        shaping_7 = False
        shaping_8 = False
        shaping_9 = False
    elif key_resp.keys == "num_5":
        shaping_1 = False
        shaping_2 = False
        shaping_3 = False
        shaping_4 = False
        shaping_5 = True
        shaping_6 = False
        shaping_7 = False
        shaping_8 = False
        shaping_9 = False
    elif key_resp.keys == "num_6":
        shaping_1 = False
        shaping_2 = False
        shaping_3 = False
        shaping_4 = False
        shaping_5 = False
        shaping_6 = True
        shaping_7 = False
        shaping_8 = False
        shaping_9 = False
    elif key_resp.keys == "num_7":
        shaping_1 = False
        shaping_2 = False
        shaping_3 = False
        shaping_4 = False
        shaping_5 = False
        shaping_6 = False
        shaping_7 = True
        shaping_8 = False
        shaping_9 = False
    elif key_resp.keys == "num_8":
        shaping_1 = False
        shaping_2 = False
        shaping_3 = False
        shaping_4 = False
        shaping_5 = False
        shaping_6 = False
        shaping_7 = False
        shaping_8 = True
        shaping_9 = False
    elif key_resp.keys == "num_9":
        shaping_1 = False
        shaping_2 = False
        shaping_3 = False
        shaping_4 = False
        shaping_5 = False
        shaping_6 = False
        shaping_7 = False
        shaping_8 = False
        shaping_9 = True
    
    # the Routine "response" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # --- Prepare to start Routine "shaping" ---
    continueRoutine = True
    # update component parameters for each repeat
    # Run 'Begin Routine' code from cd
    
    #randomizing the shaping phase
    if shaping_1 == True:
        #using for how many lines presented
        pos1= 0
        pos2 = 9999
        pos3 = 9999
        pos4 = 9999
        pos5 = 9999
        pos6 = 9999
        pos7 = 9999
        pos8 = 9999
        pos9 = 9999
        #randomize the widths
        width = np.arange(0.2, 0.6, 0.05)
        width_list = list(width)
    
        random.shuffle(width_list)
        x = float(f"{width_list[0]:.2f}")
    elif shaping_2 == True:
        pos3 = 9999
        pos4 = 9999
        pos5 = 9999
        pos6 = 9999
        pos7 = 9999
        pos8 = 9999
        pos9 = 9999
        def get_random_tuple():
            width = np.arange(0.2, 0.6, 0.05)
            width_list = list(width)
    
            random.shuffle(width_list)
            x = float(f"{width_list[0]:.2f}")
        #randomize the spaces
        #making 2 list of x coordinates 
            pos1_list = [-0.8, -0.7, -0.6, -0.5, -0.4, -0.3,-.2]
            pos2_list = [0.8, 0.7, 0.6, 0.5, 0.4, 0.3,0.2]
            toplu_liste = []
            #pair this values with several lists and randomly choosing one of them
            for i in range(len(pos1_list)):
                toplu_liste.append((pos1_list[i], pos2_list[i]))
    
            selected_list = random.choice(toplu_liste)
    
            pos1 = selected_list[0]
            pos2 = selected_list[1]
            if abs(pos2 - pos1) <= x:
                return get_random_tuple()
    
            return x, pos1, pos2
    
        x, pos1, pos2 = get_random_tuple()
    elif shaping_3 == True:
        pos4 = 9999
        pos5 = 9999
        pos6 = 9999
        pos7 = 9999
        pos8 = 9999
        pos9 = 9999
    #new function for randomization and prevent collapsing
        def get_random_tuple():
            width = np.arange(0.2, 0.6, 0.05)
            width_list = list(width)
    
            random.shuffle(width_list)
            x = float(f"{width_list[0]:.2f}")
    
            pos1_list = [-1.3, -1.2, -1.1, -1, -.9, -.8, -.7, -.6, -.5, -.4, -.3]
            pos2_list = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
            pos3_list = [1.3, 1.2, 1.1, 1, .9, .8, .7, .6, .5, .4, .3]
            toplu_liste = []
            for i in range(len(pos1_list)):
                toplu_liste.append((pos1_list[i], pos2_list[i], pos3_list[i]))
    
            selected_list = random.choice(toplu_liste)
    
            pos1 = selected_list[0]
            pos2 = selected_list[1]
            pos3 = selected_list[2]
    #prevent collapsing. if it is collapsing re-run the code
            if abs(pos2 - pos1) <= x:
                return get_random_tuple()
    
            return x, pos1, pos2, pos3
    
        x, pos1, pos2, pos3 = get_random_tuple()
    #it goes to nine lines same principle
    elif shaping_4 == True:
        pos5 = 9999
        pos6 = 9999
        pos7 = 9999
        pos8 = 9999
        pos9 = 9999
        def get_random_tuple():
            width = np.arange(0.2, 0.6, 0.05)
            width_list = list(width)
    
            random.shuffle(width_list)
            x = float(f"{width_list[0]:.2f}")
    
            pos1_list = [-0.45, -0.6, -0.75, -0.9, -1.05, -1.2, -1.35, -1.5, -1.65, -1.8, -1.95]
            pos2_list = [-0.15, -0.2, -0.25, -0.3, -0.35, -0.4, -0.45, -0.5, -0.55, -0.6, -0.65]
            pos3_list = [0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65]
            pos4_list = [0.45, 0.6, 0.75, 0.9, 1.05, 1.2, 1.35, 1.5, 1.65, 1.8, 1.95]
            toplu_liste = []
            for i in range(len(pos1_list)):
                toplu_liste.append((pos1_list[i], pos2_list[i], pos3_list[i], pos4_list[i]))
    
            selected_list = random.choice(toplu_liste)
    
            pos1 = selected_list[0]
            pos2 = selected_list[1]
            pos3 = selected_list[2]
            pos4 = selected_list[3]
    
            if abs(pos2 - pos1) <= x:
                return get_random_tuple()
    
            return x, pos1, pos2, pos3, pos4
    
        x, pos1, pos2, pos3, pos4 = get_random_tuple()
    elif shaping_5 == True:
        pos6 = 9999
        pos7 = 9999
        pos8 = 9999
        pos9 = 9999
        def get_random_tuple():
            width = np.arange(0.2, 0.6, 0.05)
            width_list = list(width)
    
            random.shuffle(width_list)
            x = float(f"{width_list[0]:.2f}")
    
            pos1_list = [-0.6, -0.8, -1, -1.2, -1.4, -1.6, -1.8, -2, -2.2, -2.4, -2.6]
            pos2_list = [-0.3, -0.4, -0.5, -0.6, -0.7, -0.8, -0.9, -1, -1.1, -1.2, -1.3]
            pos3_list = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
            pos4_list = [0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1, 1.2, 1.3]
            pos5_list = [0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2, 2.2, 2.4, 2.6]
    
            toplu_liste = []
            for i in range(len(pos1_list)):
                toplu_liste.append((pos1_list[i], pos2_list[i], pos3_list[i], pos4_list[i], pos5_list[i]))
    
            selected_list = random.choice(toplu_liste)
    
            pos1 = float(f"{selected_list[0]:.2f}")
            pos2 = float(f"{selected_list[1]:.2f}")
            pos3 = float(f"{selected_list[2]:.2f}")
            pos4 = float(f"{selected_list[3]:.2f}")
            pos5 = float(f"{selected_list[4]:.2f}")
    
            if abs(pos2 - pos1) <= x:
                return get_random_tuple()
    
            return x, pos1, pos2, pos3, pos4, pos5
    
        x, pos1, pos2, pos3, pos4, pos5 = get_random_tuple()
    elif shaping_6 == True:
        pos7 = 9999
        pos8 = 9999
        pos9 = 9999
        def get_random_tuple():
            width = np.arange(0.2, 0.6, 0.05)
            width_list = list(width)
    
            random.shuffle(width_list)
            x = float(f"{width_list[0]:.2f}")
    
            pos1_list = [-0.75, -1, -1.25, -1.5, -1.75, -2, -2.25, -2.5, -2.75, -3, -3.25]
            pos2_list = [-0.45, -0.6, -0.75, -0.9, -1.05, -1.2, -1.35, -1.5, -1.65, -1.8, -1.95]
            pos3_list = [-0.15, -0.2, -0.25, -0.3, -0.35, -0.4, -0.45, -0.5, -0.55, -0.6, -0.65]
            pos4_list = [0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65]
            pos5_list = [0.45, 0.6, 0.75, 0.9, 1.05, 1.2, 1.35, 1.5, 1.65, 1.8, 1.95]
            pos6_list = [0.75, 1, 1.25, 1.5, 1.75, 2, 2.25, 2.5, 2.75, 3, 3.25]
    
            toplu_liste = []
            for i in range(len(pos1_list)):
                toplu_liste.append((pos1_list[i], pos2_list[i], pos3_list[i], pos4_list[i], pos5_list[i],pos6_list[i]))
    
            selected_list = random.choice(toplu_liste)
    
            pos1 = float(f"{selected_list[0]:.2f}")
            pos2 = float(f"{selected_list[1]:.2f}")
            pos3 = float(f"{selected_list[2]:.2f}")
            pos4 = float(f"{selected_list[3]:.2f}")
            pos5 = float(f"{selected_list[4]:.2f}")
            pos6 = float(f"{selected_list[5]:.2f}")
            
            if abs(pos2 - pos1) <= x:
                return get_random_tuple()
    
            return x, pos1, pos2, pos3, pos4, pos5, pos6
    
        x, pos1, pos2, pos3, pos4, pos5, pos6 = get_random_tuple()
    elif shaping_7 == True:
        pos8 = 9999
        pos9 = 9999
        def get_random_tuple():
            width = np.arange(0.2, 0.6, 0.05)
            width_list = list(width)
    
            random.shuffle(width_list)
            x = float(f"{width_list[0]:.2f}")
    
            pos1_list = [-0.9, -1.2, -1.5, -1.8, -2.1, -2.4, -2.7, -3, -3.3, -3.6, -3.9]
            pos2_list = [-0.6, -0.8, -1, -1.2, -1.4, -1.6, -1.8, -2, -2.2, -2.4, -2.6]
            pos3_list = [-0.3, -0.4, -0.5, -0.6, -0.7, -0.8, -0.9, -1, -1.1, -1.2, -1.3]
            pos4_list = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
            pos5_list = [0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1, 1.2, 1.3]
            pos6_list = [0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2, 2.2, 2.4, 2.6]
            pos7_list = [0.9, 1.2, 1.5, 1.8, 2.1, 2.4, 2.7, 3, 3.3, 3.6, 3.9]
    
    
            toplu_liste = []
            for i in range(len(pos1_list)):
                toplu_liste.append((pos1_list[i], pos2_list[i], pos3_list[i], pos4_list[i], pos5_list[i],pos6_list[i],pos7_list[i]))
    
            selected_list = random.choice(toplu_liste)
    
            pos1 = float(f"{selected_list[0]:.2f}")
            pos2 = float(f"{selected_list[1]:.2f}")
            pos3 = float(f"{selected_list[2]:.2f}")
            pos4 = float(f"{selected_list[3]:.2f}")
            pos5 = float(f"{selected_list[4]:.2f}")
            pos6 = float(f"{selected_list[5]:.2f}")
            pos7 = float(f"{selected_list[6]:.2f}")
            if abs(pos2 - pos1) <= x:
                return get_random_tuple()
    
            return x, pos1, pos2, pos3, pos4, pos5, pos6, pos7
    
        x, pos1, pos2, pos3, pos4, pos5, pos6, pos7 = get_random_tuple()
    elif shaping_8 == True:
        pos9 = 9999
        def get_random_tuple():
            width = np.arange(0.2, 0.6, 0.05)
            width_list = list(width)
    
            random.shuffle(width_list)
            x = float(f"{width_list[0]:.2f}")
    
            pos1_list = [-1.2, -1.6, -2, -2.4, -2.8, -3.2, -3.6, -4, -4.4, -4.8, -5.2]
            pos2_list = [-0.9, -1.2, -1.5, -1.8, -2.1, -2.4, -2.7, -3, -3.3, -3.6, -3.9]
            pos3_list = [-0.6, -0.8, -1, -1.2, -1.4, -1.6, -1.8, -2, -2.2, -2.4, -2.6]
            pos4_list = [-0.3, -0.4, -0.5, -0.6, -0.7, -0.8, -0.9, -1, -1.1, -1.2, -1.3]
            pos5_list = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
            pos6_list = [0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1, 1.2, 1.3]
            pos7_list = [0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2, 2.2, 2.4, 2.6]
            pos8_list = [0.9, 1.2, 1.5, 1.8, 2.1, 2.4, 2.7, 3, 3.3, 3.6, 3.9]
            pos9_list = [1.2, 1.6, 2, 2.4, 2.8, 3.2, 3.6, 4, 4.4, 4.8, 5.2]
    
    
    
            toplu_liste = []
            for i in range(len(pos1_list)):
                toplu_liste.append((pos1_list[i], pos2_list[i], pos3_list[i], pos4_list[i], pos5_list[i],pos6_list[i],pos7_list[i],pos8_list[i]))
    
            selected_list = random.choice(toplu_liste)
    
            pos1 = float(f"{selected_list[0]:.2f}")
            pos2 = float(f"{selected_list[1]:.2f}")
            pos3 = float(f"{selected_list[2]:.2f}")
            pos4 = float(f"{selected_list[3]:.2f}")
            pos5 = float(f"{selected_list[4]:.2f}")
            pos6 = float(f"{selected_list[5]:.2f}")
            pos7 = float(f"{selected_list[6]:.2f}")
            pos8 = float(f"{selected_list[7]:.2f}")
            if abs(pos2 - pos1) <= x:
                return get_random_tuple()
    
            return x, pos1, pos2, pos3, pos4, pos5, pos6, pos7,pos8
    
        x, pos1, pos2, pos3, pos4, pos5, pos6, pos7,pos8 = get_random_tuple()
    elif shaping_9 == True:
        def get_random_tuple():
            width = np.arange(0.2, 0.6, 0.05)
            width_list = list(width)
    
            random.shuffle(width_list)
            x = float(f"{width_list[0]:.2f}")
    
            pos1_list = [-1.2, -1.6, -2, -2.4, -2.8, -3.2, -3.6, -4, -4.4, -4.8, -5.2]
            pos2_list = [-0.9, -1.2, -1.5, -1.8, -2.1, -2.4, -2.7, -3, -3.3, -3.6, -3.9]
            pos3_list = [-0.6, -0.8, -1, -1.2, -1.4, -1.6, -1.8, -2, -2.2, -2.4, -2.6]
            pos4_list = [-0.3, -0.4, -0.5, -0.6, -0.7, -0.8, -0.9, -1, -1.1, -1.2, -1.3]
            pos5_list = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
            pos6_list = [0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1, 1.2, 1.3]
            pos7_list = [0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2, 2.2, 2.4, 2.6]
            pos8_list = [0.9, 1.2, 1.5, 1.8, 2.1, 2.4, 2.7, 3, 3.3, 3.6, 3.9]
            pos9_list = [1.2, 1.6, 2, 2.4, 2.8, 3.2, 3.6, 4, 4.4, 4.8, 5.2]
    
    
            toplu_liste = []
            for i in range(len(pos1_list)):
                toplu_liste.append((pos1_list[i], pos2_list[i], pos3_list[i], pos4_list[i], pos5_list[i],pos6_list[i],pos7_list[i],pos8_list[i],pos9_list[i]))
    
            selected_list = random.choice(toplu_liste)
    
            pos1 = float(f"{selected_list[0]:.2f}")
            pos2 = float(f"{selected_list[1]:.2f}")
            pos3 = float(f"{selected_list[2]:.2f}")
            pos4 = float(f"{selected_list[3]:.2f}")
            pos5 = float(f"{selected_list[4]:.2f}")
            pos6 = float(f"{selected_list[5]:.2f}")
            pos7 = float(f"{selected_list[6]:.2f}")
            pos8 = float(f"{selected_list[7]:.2f}")
            pos9 = float(f"{selected_list[8]:.2f}")
            if pos2 - pos1 <= x:
                return get_random_tuple()
    
            return x, pos1, pos2, pos3, pos4, pos5, pos6, pos7,pos8,pos9
    
        x, pos1, pos2, pos3, pos4, pos5, pos6, pos7,pos8,pos9 = get_random_tuple()
        
    starting_space = round(abs(pos2-pos1),2)
    thisExp.addData("starting_width_deg", x)
    thisExp.addData("starting_space_deg", starting_space)
    next_5.keys = []
    next_5.rt = []
    _next_5_allKeys = []
    # keep track of which components have finished
    shapingComponents = [test_p1_5, test_p2_5, test_p3_5, test_p4_5, test_p5_5, test_p6, test_p7, test_p8, test_p9, next_5]
    for thisComponent in shapingComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    frameN = -1
    
    # --- Run Routine "shaping" ---
    routineForceEnded = not continueRoutine
    while continueRoutine:
        # get current time
        t = routineTimer.getTime()
        tThisFlip = win.getFutureFlipTime(clock=routineTimer)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        # Run 'Each Frame' code from cd
        
        #adjusting the widths and coordinates
        if shaping_1 == True:
            for key in event.getKeys():
                if key == thinner:
                        x -= 0.05 
                elif key == thicker:
                        x += 0.05
        elif shaping_2 == True:
            for key in event.getKeys():
                if key == thinner:
                        x -= 0.05  
                elif key == thicker:
                        x += 0.05     
                elif key == narrower:
                        pos1 += 0.05 
                        pos2 -= 0.05 
                elif key == wider:
                        pos1 -= 0.05 
                        pos2 += 0.05 
        elif shaping_3 == True:
            for key in event.getKeys():
                if key == thinner:
                        x -= 0.05 
                elif key == thicker:
                        x += 0.05     
                elif key == wider:
                        pos1 -= 0.05 
                        pos3 += 0.05 
                elif key == narrower:
                        pos1 += 0.05 
                        pos3 -= 0.05 
        elif shaping_4 == True:
            for key in event.getKeys():
                if key == thinner:
                        x -= 0.05 
                elif key == thicker:
                        x += 0.05    
                elif key == wider:
                        pos1 -= 0.15
                        pos2 -= 0.05
                        pos3 += 0.05
                        pos4 += 0.15
                elif key == narrower:
                        pos1 += 0.15
                        pos2 += 0.05
                        pos3 -= 0.05
                        pos4 -= 0.15
        elif shaping_5 == True:
            for key in event.getKeys():
                if key == thinner:
                        x -= 0.05 
                elif key == thicker:
                        x += 0.05  
                elif key == wider:
                        pos1 -= 0.1
                        pos2 -= 0.05
                        pos4 += 0.05
                        pos5 += 0.1
                elif key == narrower:
                        pos1 += 0.1
                        pos2 += 0.05
                        pos4 -= 0.05
                        pos5 -= 0.1
        elif shaping_6 == True:
            for key in event.getKeys():
                if key == thinner:
                        x -= 0.05 
                elif key == thicker:
                        x += 0.05    
                elif key == wider:
                        pos1 -= 0.25
                        pos2 -= 0.15
                        pos3 -= 0.05
                        pos4 += 0.05
                        pos5 += 0.15
                        pos6 += 0.25
                elif key == narrower:
                        pos1 += 0.25
                        pos2 += 0.15
                        pos3 += 0.05
                        pos4 -= 0.05
                        pos5 -= 0.15
                        pos6 -= 0.25
        elif shaping_7 == True:
            for key in event.getKeys():
                if key == thinner:
                        x -= 0.05 
                elif key == thicker:
                        x += 0.05  
                elif key == wider:
                        pos1 -= 0.15
                        pos2 -= 0.1
                        pos3 -= 0.05
                        pos5 += 0.05
                        pos6 += 0.1
                        pos7 += 0.15
                elif key == narrower:
                        pos1 += 0.15
                        pos2 += 0.1
                        pos3 += 0.05
                        pos5 -= 0.05
                        pos6 -= 0.1
                        pos7 -= 0.15
        elif shaping_8 == True:
            for key in event.getKeys():
                if key == thinner:
                        x -= 0.05 
                elif key == thicker:
                        x += 0.05    
                elif key == wider:
                        pos1 -= 0.35
                        pos2 -= 0.25
                        pos3 -= 0.15
                        pos4 -= 0.05
                        pos5 += 0.05
                        pos6 += 0.15
                        pos7 += 0.25
                        pos8 += 0.35 
                elif key == narrower:
                        pos1 += 0.35
                        pos2 += 0.25
                        pos3 += 0.15
                        pos4 += 0.05
                        pos5 -= 0.05
                        pos6 -= 0.15
                        pos7 -= 0.25
                        pos8 -= 0.35  
        elif shaping_9 == True:
            for key in event.getKeys():
                if key == thinner:
                        x -= 0.05 
                elif key == thicker:
                        x += 0.05  
                elif key == wider:
                        pos1 -= 0.2
                        pos2 -= 0.15
                        pos3 -= 0.1
                        pos4 -= 0.05
                        pos6 += 0.05
                        pos7 += 0.1
                        pos8 += 0.15
                        pos9 += 0.2
                elif key == narrower:
                        pos1 += 0.2
                        pos2 += 0.15
                        pos3 += 0.1
                        pos4 += 0.05
                        pos6 -= 0.05
                        pos7 -= 0.1
                        pos8 -= 0.15
                        pos9 -= 0.2
        
        # *test_p1_5* updates
        
        # if test_p1_5 is starting this frame...
        if test_p1_5.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            test_p1_5.frameNStart = frameN  # exact frame index
            test_p1_5.tStart = t  # local t and not account for scr refresh
            test_p1_5.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(test_p1_5, 'tStartRefresh')  # time at next scr refresh
            # update status
            test_p1_5.status = STARTED
            test_p1_5.setAutoDraw(True)
        
        # if test_p1_5 is active this frame...
        if test_p1_5.status == STARTED:
            # update params
            test_p1_5.setFillColor(renk_p1, log=False)
            test_p1_5.setPos((pos1, 0), log=False)
            test_p1_5.setSize((x, 1.9), log=False)
            test_p1_5.setLineColor(renk_p1, log=False)
        
        # *test_p2_5* updates
        
        # if test_p2_5 is starting this frame...
        if test_p2_5.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            test_p2_5.frameNStart = frameN  # exact frame index
            test_p2_5.tStart = t  # local t and not account for scr refresh
            test_p2_5.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(test_p2_5, 'tStartRefresh')  # time at next scr refresh
            # update status
            test_p2_5.status = STARTED
            test_p2_5.setAutoDraw(True)
        
        # if test_p2_5 is active this frame...
        if test_p2_5.status == STARTED:
            # update params
            test_p2_5.setFillColor(renk_p2, log=False)
            test_p2_5.setPos((pos2, 0), log=False)
            test_p2_5.setSize((x, 1.9), log=False)
            test_p2_5.setLineColor(renk_p2, log=False)
        
        # *test_p3_5* updates
        
        # if test_p3_5 is starting this frame...
        if test_p3_5.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            test_p3_5.frameNStart = frameN  # exact frame index
            test_p3_5.tStart = t  # local t and not account for scr refresh
            test_p3_5.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(test_p3_5, 'tStartRefresh')  # time at next scr refresh
            # update status
            test_p3_5.status = STARTED
            test_p3_5.setAutoDraw(True)
        
        # if test_p3_5 is active this frame...
        if test_p3_5.status == STARTED:
            # update params
            test_p3_5.setFillColor(renk_p3, log=False)
            test_p3_5.setPos((pos3, 0), log=False)
            test_p3_5.setSize((x, 1.9), log=False)
            test_p3_5.setLineColor(renk_p3, log=False)
        
        # *test_p4_5* updates
        
        # if test_p4_5 is starting this frame...
        if test_p4_5.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            test_p4_5.frameNStart = frameN  # exact frame index
            test_p4_5.tStart = t  # local t and not account for scr refresh
            test_p4_5.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(test_p4_5, 'tStartRefresh')  # time at next scr refresh
            # update status
            test_p4_5.status = STARTED
            test_p4_5.setAutoDraw(True)
        
        # if test_p4_5 is active this frame...
        if test_p4_5.status == STARTED:
            # update params
            test_p4_5.setFillColor(renk_p4, log=False)
            test_p4_5.setPos((pos4, 0), log=False)
            test_p4_5.setSize((x, 1.9), log=False)
            test_p4_5.setLineColor(renk_p4, log=False)
        
        # *test_p5_5* updates
        
        # if test_p5_5 is starting this frame...
        if test_p5_5.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            test_p5_5.frameNStart = frameN  # exact frame index
            test_p5_5.tStart = t  # local t and not account for scr refresh
            test_p5_5.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(test_p5_5, 'tStartRefresh')  # time at next scr refresh
            # update status
            test_p5_5.status = STARTED
            test_p5_5.setAutoDraw(True)
        
        # if test_p5_5 is active this frame...
        if test_p5_5.status == STARTED:
            # update params
            test_p5_5.setFillColor(renk_p5, log=False)
            test_p5_5.setPos((pos5, 0), log=False)
            test_p5_5.setSize((x, 1.9), log=False)
            test_p5_5.setLineColor(renk_p5, log=False)
        
        # *test_p6* updates
        
        # if test_p6 is starting this frame...
        if test_p6.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            test_p6.frameNStart = frameN  # exact frame index
            test_p6.tStart = t  # local t and not account for scr refresh
            test_p6.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(test_p6, 'tStartRefresh')  # time at next scr refresh
            # update status
            test_p6.status = STARTED
            test_p6.setAutoDraw(True)
        
        # if test_p6 is active this frame...
        if test_p6.status == STARTED:
            # update params
            test_p6.setFillColor(renk_p5, log=False)
            test_p6.setPos((pos6, 0), log=False)
            test_p6.setSize((x, 1.9), log=False)
            test_p6.setLineColor(renk_p5, log=False)
        
        # *test_p7* updates
        
        # if test_p7 is starting this frame...
        if test_p7.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            test_p7.frameNStart = frameN  # exact frame index
            test_p7.tStart = t  # local t and not account for scr refresh
            test_p7.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(test_p7, 'tStartRefresh')  # time at next scr refresh
            # update status
            test_p7.status = STARTED
            test_p7.setAutoDraw(True)
        
        # if test_p7 is active this frame...
        if test_p7.status == STARTED:
            # update params
            test_p7.setFillColor(renk_p5, log=False)
            test_p7.setPos((pos7, 0), log=False)
            test_p7.setSize((x, 1.9), log=False)
            test_p7.setLineColor(renk_p5, log=False)
        
        # *test_p8* updates
        
        # if test_p8 is starting this frame...
        if test_p8.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            test_p8.frameNStart = frameN  # exact frame index
            test_p8.tStart = t  # local t and not account for scr refresh
            test_p8.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(test_p8, 'tStartRefresh')  # time at next scr refresh
            # update status
            test_p8.status = STARTED
            test_p8.setAutoDraw(True)
        
        # if test_p8 is active this frame...
        if test_p8.status == STARTED:
            # update params
            test_p8.setFillColor(renk_p5, log=False)
            test_p8.setPos((pos8, 0), log=False)
            test_p8.setSize((x, 1.9), log=False)
            test_p8.setLineColor(renk_p5, log=False)
        
        # *test_p9* updates
        
        # if test_p9 is starting this frame...
        if test_p9.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            test_p9.frameNStart = frameN  # exact frame index
            test_p9.tStart = t  # local t and not account for scr refresh
            test_p9.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(test_p9, 'tStartRefresh')  # time at next scr refresh
            # update status
            test_p9.status = STARTED
            test_p9.setAutoDraw(True)
        
        # if test_p9 is active this frame...
        if test_p9.status == STARTED:
            # update params
            test_p9.setFillColor(renk_p5, log=False)
            test_p9.setPos((pos9, 0), log=False)
            test_p9.setSize((x, 1.9), log=False)
            test_p9.setLineColor(renk_p5, log=False)
        
        # *next_5* updates
        waitOnFlip = False
        
        # if next_5 is starting this frame...
        if next_5.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            next_5.frameNStart = frameN  # exact frame index
            next_5.tStart = t  # local t and not account for scr refresh
            next_5.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(next_5, 'tStartRefresh')  # time at next scr refresh
            # update status
            next_5.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(next_5.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(next_5.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if next_5.status == STARTED and not waitOnFlip:
            theseKeys = next_5.getKeys(keyList=['return'], waitRelease=False)
            _next_5_allKeys.extend(theseKeys)
            if len(_next_5_allKeys):
                next_5.keys = _next_5_allKeys[-1].name  # just the last key pressed
                next_5.rt = _next_5_allKeys[-1].rt
                # a response ends the routine
                continueRoutine = False
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            routineForceEnded = True
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in shapingComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # --- Ending Routine "shaping" ---
    for thisComponent in shapingComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # Run 'End Routine' code from cd
    #spacing variable
    space_deg = abs(round((pos2 - pos1), 2))
    #adding to the data
    thisExp.addData("response_width_degree",x)
    thisExp.addData("response_spacing_degree",space_deg)
    
    #converting degrees to pixels
    x_pixel = round(deg2pix(x, monitor = win.monitor), 5)
    space_pixel = round(deg2pix(space_deg, monitor = win.monitor), 5)
    #adding to the data
    thisExp.addData("response_width_pixel",x_pixel)
    thisExp.addData("response_spacing_pixel",space_pixel)
    
    
    # check responses
    if next_5.keys in ['', [], None]:  # No response was made
        next_5.keys = None
    trials.addData('next_5.keys',next_5.keys)
    if next_5.keys != None:  # we had a response
        trials.addData('next_5.rt', next_5.rt)
    # the Routine "shaping" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    thisExp.nextEntry()
    
# completed 1.0 repeats of 'trials'


# --- Prepare to start Routine "intro_2" ---
continueRoutine = True
# update component parameters for each repeat
key_resp_4.keys = []
key_resp_4.rt = []
_key_resp_4_allKeys = []
# keep track of which components have finished
intro_2Components = [text, key_resp_4]
for thisComponent in intro_2Components:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
frameN = -1

# --- Run Routine "intro_2" ---
routineForceEnded = not continueRoutine
while continueRoutine:
    # get current time
    t = routineTimer.getTime()
    tThisFlip = win.getFutureFlipTime(clock=routineTimer)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *text* updates
    
    # if text is starting this frame...
    if text.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        text.frameNStart = frameN  # exact frame index
        text.tStart = t  # local t and not account for scr refresh
        text.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(text, 'tStartRefresh')  # time at next scr refresh
        # update status
        text.status = STARTED
        text.setAutoDraw(True)
    
    # if text is active this frame...
    if text.status == STARTED:
        # update params
        pass
    
    # *key_resp_4* updates
    waitOnFlip = False
    
    # if key_resp_4 is starting this frame...
    if key_resp_4.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        key_resp_4.frameNStart = frameN  # exact frame index
        key_resp_4.tStart = t  # local t and not account for scr refresh
        key_resp_4.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(key_resp_4, 'tStartRefresh')  # time at next scr refresh
        # update status
        key_resp_4.status = STARTED
        # keyboard checking is just starting
        waitOnFlip = True
        win.callOnFlip(key_resp_4.clock.reset)  # t=0 on next screen flip
        win.callOnFlip(key_resp_4.clearEvents, eventType='keyboard')  # clear events on next screen flip
    if key_resp_4.status == STARTED and not waitOnFlip:
        theseKeys = key_resp_4.getKeys(keyList=["c"], waitRelease=False)
        _key_resp_4_allKeys.extend(theseKeys)
        if len(_key_resp_4_allKeys):
            key_resp_4.keys = _key_resp_4_allKeys[-1].name  # just the last key pressed
            key_resp_4.rt = _key_resp_4_allKeys[-1].rt
            # a response ends the routine
            continueRoutine = False
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        routineForceEnded = True
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in intro_2Components:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# --- Ending Routine "intro_2" ---
for thisComponent in intro_2Components:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
# check responses
if key_resp_4.keys in ['', [], None]:  # No response was made
    key_resp_4.keys = None
thisExp.addData('key_resp_4.keys',key_resp_4.keys)
if key_resp_4.keys != None:  # we had a response
    thisExp.addData('key_resp_4.rt', key_resp_4.rt)
thisExp.nextEntry()
# the Routine "intro_2" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# set up handler to look after randomisation of conditions etc
blocks = data.TrialHandler(nReps=6.0, method='sequential', 
    extraInfo=expInfo, originPath=-1,
    trialList=[None],
    seed=None, name='blocks')
thisExp.addLoop(blocks)  # add the loop to the experiment
thisBlock = blocks.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb = thisBlock.rgb)
if thisBlock != None:
    for paramName in thisBlock:
        exec('{} = thisBlock[paramName]'.format(paramName))

for thisBlock in blocks:
    currentLoop = blocks
    # abbreviate parameter names if possible (e.g. rgb = thisBlock.rgb)
    if thisBlock != None:
        for paramName in thisBlock:
            exec('{} = thisBlock[paramName]'.format(paramName))
    
    # set up handler to look after randomisation of conditions etc
    loop = data.TrialHandler(nReps=1.0, method='random', 
        extraInfo=expInfo, originPath=-1,
        trialList=data.importConditions('parametersV2.xlsx'),
        seed=None, name='loop')
    thisExp.addLoop(loop)  # add the loop to the experiment
    thisLoop = loop.trialList[0]  # so we can initialise stimuli with some values
    # abbreviate parameter names if possible (e.g. rgb = thisLoop.rgb)
    if thisLoop != None:
        for paramName in thisLoop:
            exec('{} = thisLoop[paramName]'.format(paramName))
    
    for thisLoop in loop:
        currentLoop = loop
        # abbreviate parameter names if possible (e.g. rgb = thisLoop.rgb)
        if thisLoop != None:
            for paramName in thisLoop:
                exec('{} = thisLoop[paramName]'.format(paramName))
        
        # --- Prepare to start Routine "interval" ---
        continueRoutine = True
        # update component parameters for each repeat
        # Run 'Begin Routine' code from code_5
        random_n = [1,1.25,1.5,1.75,2]
        shuffle(random_n)
        # keep track of which components have finished
        intervalComponents = [fixation_interval]
        for thisComponent in intervalComponents:
            thisComponent.tStart = None
            thisComponent.tStop = None
            thisComponent.tStartRefresh = None
            thisComponent.tStopRefresh = None
            if hasattr(thisComponent, 'status'):
                thisComponent.status = NOT_STARTED
        # reset timers
        t = 0
        _timeToFirstFrame = win.getFutureFlipTime(clock="now")
        frameN = -1
        
        # --- Run Routine "interval" ---
        routineForceEnded = not continueRoutine
        while continueRoutine:
            # get current time
            t = routineTimer.getTime()
            tThisFlip = win.getFutureFlipTime(clock=routineTimer)
            tThisFlipGlobal = win.getFutureFlipTime(clock=None)
            frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
            # update/draw components on each frame
            
            # *fixation_interval* updates
            
            # if fixation_interval is starting this frame...
            if fixation_interval.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                fixation_interval.frameNStart = frameN  # exact frame index
                fixation_interval.tStart = t  # local t and not account for scr refresh
                fixation_interval.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(fixation_interval, 'tStartRefresh')  # time at next scr refresh
                # update status
                fixation_interval.status = STARTED
                fixation_interval.setAutoDraw(True)
            
            # if fixation_interval is active this frame...
            if fixation_interval.status == STARTED:
                # update params
                pass
            
            # if fixation_interval is stopping this frame...
            if fixation_interval.status == STARTED:
                # is it time to stop? (based on global clock, using actual start)
                if tThisFlipGlobal > fixation_interval.tStartRefresh + random_n[0]-frameTolerance:
                    # keep track of stop time/frame for later
                    fixation_interval.tStop = t  # not accounting for scr refresh
                    fixation_interval.frameNStop = frameN  # exact frame index
                    # update status
                    fixation_interval.status = FINISHED
                    fixation_interval.setAutoDraw(False)
            
            # check for quit (typically the Esc key)
            if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
                core.quit()
            
            # check if all components have finished
            if not continueRoutine:  # a component has requested a forced-end of Routine
                routineForceEnded = True
                break
            continueRoutine = False  # will revert to True if at least one component still running
            for thisComponent in intervalComponents:
                if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                    continueRoutine = True
                    break  # at least one component has not yet finished
            
            # refresh the screen
            if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                win.flip()
        
        # --- Ending Routine "interval" ---
        for thisComponent in intervalComponents:
            if hasattr(thisComponent, "setAutoDraw"):
                thisComponent.setAutoDraw(False)
        # the Routine "interval" was not non-slip safe, so reset the non-slip timer
        routineTimer.reset()
        
        # --- Prepare to start Routine "stimuli" ---
        continueRoutine = True
        # update component parameters for each repeat
        # Run 'Begin Routine' code from code
        x = random.random()
        
        if x < 0.5:
            posx1 = trans_posi(posx1)
            posx2 = trans_posi(posx2)
            posx3 = trans_posi(posx3)
            posx4 = trans_posi(posx4)
            posx5 = trans_posi(posx5)
        p1.setPos((posx1, 0))
        p1.setSize((w, h))
        p2.setPos((posx2, 0))
        p2.setSize((w, h))
        p3.setPos((posx3, 0))
        p3.setSize((w, h))
        p4.setPos((posx4, 0))
        p4.setSize((w, h))
        p5.setPos((posx5, 0))
        p5.setSize((w, h))
        # keep track of which components have finished
        stimuliComponents = [p1, p2, p3, p4, p5, fixation]
        for thisComponent in stimuliComponents:
            thisComponent.tStart = None
            thisComponent.tStop = None
            thisComponent.tStartRefresh = None
            thisComponent.tStopRefresh = None
            if hasattr(thisComponent, 'status'):
                thisComponent.status = NOT_STARTED
        # reset timers
        t = 0
        _timeToFirstFrame = win.getFutureFlipTime(clock="now")
        frameN = -1
        
        # --- Run Routine "stimuli" ---
        routineForceEnded = not continueRoutine
        while continueRoutine:
            # get current time
            t = routineTimer.getTime()
            tThisFlip = win.getFutureFlipTime(clock=routineTimer)
            tThisFlipGlobal = win.getFutureFlipTime(clock=None)
            frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
            # update/draw components on each frame
            
            # *p1* updates
            
            # if p1 is starting this frame...
            if p1.status == NOT_STARTED and tThisFlip >= 0-frameTolerance:
                # keep track of start time/frame for later
                p1.frameNStart = frameN  # exact frame index
                p1.tStart = t  # local t and not account for scr refresh
                p1.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(p1, 'tStartRefresh')  # time at next scr refresh
                # update status
                p1.status = STARTED
                p1.setAutoDraw(True)
            
            # if p1 is active this frame...
            if p1.status == STARTED:
                # update params
                pass
            
            # if p1 is stopping this frame...
            if p1.status == STARTED:
                if frameN >= (p1.frameNStart + line1):
                    # keep track of stop time/frame for later
                    p1.tStop = t  # not accounting for scr refresh
                    p1.frameNStop = frameN  # exact frame index
                    # update status
                    p1.status = FINISHED
                    p1.setAutoDraw(False)
            
            # *p2* updates
            
            # if p2 is starting this frame...
            if p2.status == NOT_STARTED and tThisFlip >= 0-frameTolerance:
                # keep track of start time/frame for later
                p2.frameNStart = frameN  # exact frame index
                p2.tStart = t  # local t and not account for scr refresh
                p2.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(p2, 'tStartRefresh')  # time at next scr refresh
                # update status
                p2.status = STARTED
                p2.setAutoDraw(True)
            
            # if p2 is active this frame...
            if p2.status == STARTED:
                # update params
                pass
            
            # if p2 is stopping this frame...
            if p2.status == STARTED:
                if frameN >= (p2.frameNStart + line2):
                    # keep track of stop time/frame for later
                    p2.tStop = t  # not accounting for scr refresh
                    p2.frameNStop = frameN  # exact frame index
                    # update status
                    p2.status = FINISHED
                    p2.setAutoDraw(False)
            
            # *p3* updates
            
            # if p3 is starting this frame...
            if p3.status == NOT_STARTED and tThisFlip >= 0-frameTolerance:
                # keep track of start time/frame for later
                p3.frameNStart = frameN  # exact frame index
                p3.tStart = t  # local t and not account for scr refresh
                p3.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(p3, 'tStartRefresh')  # time at next scr refresh
                # update status
                p3.status = STARTED
                p3.setAutoDraw(True)
            
            # if p3 is active this frame...
            if p3.status == STARTED:
                # update params
                pass
            
            # if p3 is stopping this frame...
            if p3.status == STARTED:
                if frameN >= (p3.frameNStart + line3):
                    # keep track of stop time/frame for later
                    p3.tStop = t  # not accounting for scr refresh
                    p3.frameNStop = frameN  # exact frame index
                    # update status
                    p3.status = FINISHED
                    p3.setAutoDraw(False)
            
            # *p4* updates
            
            # if p4 is starting this frame...
            if p4.status == NOT_STARTED and tThisFlip >= 0-frameTolerance:
                # keep track of start time/frame for later
                p4.frameNStart = frameN  # exact frame index
                p4.tStart = t  # local t and not account for scr refresh
                p4.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(p4, 'tStartRefresh')  # time at next scr refresh
                # update status
                p4.status = STARTED
                p4.setAutoDraw(True)
            
            # if p4 is active this frame...
            if p4.status == STARTED:
                # update params
                pass
            
            # if p4 is stopping this frame...
            if p4.status == STARTED:
                if frameN >= (p4.frameNStart + line4):
                    # keep track of stop time/frame for later
                    p4.tStop = t  # not accounting for scr refresh
                    p4.frameNStop = frameN  # exact frame index
                    # update status
                    p4.status = FINISHED
                    p4.setAutoDraw(False)
            
            # *p5* updates
            
            # if p5 is starting this frame...
            if p5.status == NOT_STARTED and tThisFlip >= 0-frameTolerance:
                # keep track of start time/frame for later
                p5.frameNStart = frameN  # exact frame index
                p5.tStart = t  # local t and not account for scr refresh
                p5.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(p5, 'tStartRefresh')  # time at next scr refresh
                # update status
                p5.status = STARTED
                p5.setAutoDraw(True)
            
            # if p5 is active this frame...
            if p5.status == STARTED:
                # update params
                pass
            
            # if p5 is stopping this frame...
            if p5.status == STARTED:
                if frameN >= (p5.frameNStart + line5):
                    # keep track of stop time/frame for later
                    p5.tStop = t  # not accounting for scr refresh
                    p5.frameNStop = frameN  # exact frame index
                    # update status
                    p5.status = FINISHED
                    p5.setAutoDraw(False)
            
            # *fixation* updates
            
            # if fixation is starting this frame...
            if fixation.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                fixation.frameNStart = frameN  # exact frame index
                fixation.tStart = t  # local t and not account for scr refresh
                fixation.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(fixation, 'tStartRefresh')  # time at next scr refresh
                # update status
                fixation.status = STARTED
                fixation.setAutoDraw(True)
            
            # if fixation is active this frame...
            if fixation.status == STARTED:
                # update params
                pass
            
            # if fixation is stopping this frame...
            if fixation.status == STARTED:
                if frameN >= (fixation.frameNStart + 9):
                    # keep track of stop time/frame for later
                    fixation.tStop = t  # not accounting for scr refresh
                    fixation.frameNStop = frameN  # exact frame index
                    # update status
                    fixation.status = FINISHED
                    fixation.setAutoDraw(False)
            
            # check for quit (typically the Esc key)
            if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
                core.quit()
            
            # check if all components have finished
            if not continueRoutine:  # a component has requested a forced-end of Routine
                routineForceEnded = True
                break
            continueRoutine = False  # will revert to True if at least one component still running
            for thisComponent in stimuliComponents:
                if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                    continueRoutine = True
                    break  # at least one component has not yet finished
            
            # refresh the screen
            if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                win.flip()
        
        # --- Ending Routine "stimuli" ---
        for thisComponent in stimuliComponents:
            if hasattr(thisComponent, "setAutoDraw"):
                thisComponent.setAutoDraw(False)
        # the Routine "stimuli" was not non-slip safe, so reset the non-slip timer
        routineTimer.reset()
        
        # --- Prepare to start Routine "response" ---
        continueRoutine = True
        # update component parameters for each repeat
        key_resp.keys = []
        key_resp.rt = []
        _key_resp_allKeys = []
        # keep track of which components have finished
        responseComponents = [fixation_resp, key_resp]
        for thisComponent in responseComponents:
            thisComponent.tStart = None
            thisComponent.tStop = None
            thisComponent.tStartRefresh = None
            thisComponent.tStopRefresh = None
            if hasattr(thisComponent, 'status'):
                thisComponent.status = NOT_STARTED
        # reset timers
        t = 0
        _timeToFirstFrame = win.getFutureFlipTime(clock="now")
        frameN = -1
        
        # --- Run Routine "response" ---
        routineForceEnded = not continueRoutine
        while continueRoutine:
            # get current time
            t = routineTimer.getTime()
            tThisFlip = win.getFutureFlipTime(clock=routineTimer)
            tThisFlipGlobal = win.getFutureFlipTime(clock=None)
            frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
            # update/draw components on each frame
            
            # *fixation_resp* updates
            
            # if fixation_resp is starting this frame...
            if fixation_resp.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                fixation_resp.frameNStart = frameN  # exact frame index
                fixation_resp.tStart = t  # local t and not account for scr refresh
                fixation_resp.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(fixation_resp, 'tStartRefresh')  # time at next scr refresh
                # update status
                fixation_resp.status = STARTED
                fixation_resp.setAutoDraw(True)
            
            # if fixation_resp is active this frame...
            if fixation_resp.status == STARTED:
                # update params
                pass
            
            # *key_resp* updates
            waitOnFlip = False
            
            # if key_resp is starting this frame...
            if key_resp.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                key_resp.frameNStart = frameN  # exact frame index
                key_resp.tStart = t  # local t and not account for scr refresh
                key_resp.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(key_resp, 'tStartRefresh')  # time at next scr refresh
                # update status
                key_resp.status = STARTED
                # keyboard checking is just starting
                waitOnFlip = True
                win.callOnFlip(key_resp.clock.reset)  # t=0 on next screen flip
                win.callOnFlip(key_resp.clearEvents, eventType='keyboard')  # clear events on next screen flip
            if key_resp.status == STARTED and not waitOnFlip:
                theseKeys = key_resp.getKeys(keyList=["num_1","num_2","num_3","num_4","num_5","num_6","num_7","num_8","num_9"], waitRelease=False)
                _key_resp_allKeys.extend(theseKeys)
                if len(_key_resp_allKeys):
                    key_resp.keys = _key_resp_allKeys[-1].name  # just the last key pressed
                    key_resp.rt = _key_resp_allKeys[-1].rt
                    # a response ends the routine
                    continueRoutine = False
            
            # check for quit (typically the Esc key)
            if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
                core.quit()
            
            # check if all components have finished
            if not continueRoutine:  # a component has requested a forced-end of Routine
                routineForceEnded = True
                break
            continueRoutine = False  # will revert to True if at least one component still running
            for thisComponent in responseComponents:
                if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                    continueRoutine = True
                    break  # at least one component has not yet finished
            
            # refresh the screen
            if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                win.flip()
        
        # --- Ending Routine "response" ---
        for thisComponent in responseComponents:
            if hasattr(thisComponent, "setAutoDraw"):
                thisComponent.setAutoDraw(False)
        # check responses
        if key_resp.keys in ['', [], None]:  # No response was made
            key_resp.keys = None
        loop.addData('key_resp.keys',key_resp.keys)
        if key_resp.keys != None:  # we had a response
            loop.addData('key_resp.rt', key_resp.rt)
        # Run 'End Routine' code from response_code
        thisExp.addData("response",key_resp.keys)
        #to determine the how many lines should be presented after this response
        if key_resp.keys == "num_1":
            shaping_1 = True
            shaping_2 = False
            shaping_3 = False
            shaping_4 = False
            shaping_5 = False
            shaping_6 = False
            shaping_7 = False
            shaping_8 = False
            shaping_9 = False
        elif key_resp.keys == "num_2":
            shaping_1 = False
            shaping_2 = True
            shaping_3 = False
            shaping_4 = False
            shaping_5 = False
            shaping_6 = False
            shaping_7 = False
            shaping_8 = False
            shaping_9 = False
        elif key_resp.keys == "num_3":
            shaping_1 = False
            shaping_2 = False
            shaping_3 = True
            shaping_4 = False
            shaping_5 = False
            shaping_6 = False
            shaping_7 = False
            shaping_8 = False
            shaping_9 = False
        elif key_resp.keys == "num_4":
            shaping_1 = False
            shaping_2 = False
            shaping_3 = False
            shaping_4 = True
            shaping_5 = False
            shaping_6 = False
            shaping_7 = False
            shaping_8 = False
            shaping_9 = False
        elif key_resp.keys == "num_5":
            shaping_1 = False
            shaping_2 = False
            shaping_3 = False
            shaping_4 = False
            shaping_5 = True
            shaping_6 = False
            shaping_7 = False
            shaping_8 = False
            shaping_9 = False
        elif key_resp.keys == "num_6":
            shaping_1 = False
            shaping_2 = False
            shaping_3 = False
            shaping_4 = False
            shaping_5 = False
            shaping_6 = True
            shaping_7 = False
            shaping_8 = False
            shaping_9 = False
        elif key_resp.keys == "num_7":
            shaping_1 = False
            shaping_2 = False
            shaping_3 = False
            shaping_4 = False
            shaping_5 = False
            shaping_6 = False
            shaping_7 = True
            shaping_8 = False
            shaping_9 = False
        elif key_resp.keys == "num_8":
            shaping_1 = False
            shaping_2 = False
            shaping_3 = False
            shaping_4 = False
            shaping_5 = False
            shaping_6 = False
            shaping_7 = False
            shaping_8 = True
            shaping_9 = False
        elif key_resp.keys == "num_9":
            shaping_1 = False
            shaping_2 = False
            shaping_3 = False
            shaping_4 = False
            shaping_5 = False
            shaping_6 = False
            shaping_7 = False
            shaping_8 = False
            shaping_9 = True
        
        # the Routine "response" was not non-slip safe, so reset the non-slip timer
        routineTimer.reset()
        
        # --- Prepare to start Routine "shaping" ---
        continueRoutine = True
        # update component parameters for each repeat
        # Run 'Begin Routine' code from cd
        
        #randomizing the shaping phase
        if shaping_1 == True:
            #using for how many lines presented
            pos1= 0
            pos2 = 9999
            pos3 = 9999
            pos4 = 9999
            pos5 = 9999
            pos6 = 9999
            pos7 = 9999
            pos8 = 9999
            pos9 = 9999
            #randomize the widths
            width = np.arange(0.2, 0.6, 0.05)
            width_list = list(width)
        
            random.shuffle(width_list)
            x = float(f"{width_list[0]:.2f}")
        elif shaping_2 == True:
            pos3 = 9999
            pos4 = 9999
            pos5 = 9999
            pos6 = 9999
            pos7 = 9999
            pos8 = 9999
            pos9 = 9999
            def get_random_tuple():
                width = np.arange(0.2, 0.6, 0.05)
                width_list = list(width)
        
                random.shuffle(width_list)
                x = float(f"{width_list[0]:.2f}")
            #randomize the spaces
            #making 2 list of x coordinates 
                pos1_list = [-0.8, -0.7, -0.6, -0.5, -0.4, -0.3,-.2]
                pos2_list = [0.8, 0.7, 0.6, 0.5, 0.4, 0.3,0.2]
                toplu_liste = []
                #pair this values with several lists and randomly choosing one of them
                for i in range(len(pos1_list)):
                    toplu_liste.append((pos1_list[i], pos2_list[i]))
        
                selected_list = random.choice(toplu_liste)
        
                pos1 = selected_list[0]
                pos2 = selected_list[1]
                if abs(pos2 - pos1) <= x:
                    return get_random_tuple()
        
                return x, pos1, pos2
        
            x, pos1, pos2 = get_random_tuple()
        elif shaping_3 == True:
            pos4 = 9999
            pos5 = 9999
            pos6 = 9999
            pos7 = 9999
            pos8 = 9999
            pos9 = 9999
        #new function for randomization and prevent collapsing
            def get_random_tuple():
                width = np.arange(0.2, 0.6, 0.05)
                width_list = list(width)
        
                random.shuffle(width_list)
                x = float(f"{width_list[0]:.2f}")
        
                pos1_list = [-1.3, -1.2, -1.1, -1, -.9, -.8, -.7, -.6, -.5, -.4, -.3]
                pos2_list = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
                pos3_list = [1.3, 1.2, 1.1, 1, .9, .8, .7, .6, .5, .4, .3]
                toplu_liste = []
                for i in range(len(pos1_list)):
                    toplu_liste.append((pos1_list[i], pos2_list[i], pos3_list[i]))
        
                selected_list = random.choice(toplu_liste)
        
                pos1 = selected_list[0]
                pos2 = selected_list[1]
                pos3 = selected_list[2]
        #prevent collapsing. if it is collapsing re-run the code
                if abs(pos2 - pos1) <= x:
                    return get_random_tuple()
        
                return x, pos1, pos2, pos3
        
            x, pos1, pos2, pos3 = get_random_tuple()
        #it goes to nine lines same principle
        elif shaping_4 == True:
            pos5 = 9999
            pos6 = 9999
            pos7 = 9999
            pos8 = 9999
            pos9 = 9999
            def get_random_tuple():
                width = np.arange(0.2, 0.6, 0.05)
                width_list = list(width)
        
                random.shuffle(width_list)
                x = float(f"{width_list[0]:.2f}")
        
                pos1_list = [-0.45, -0.6, -0.75, -0.9, -1.05, -1.2, -1.35, -1.5, -1.65, -1.8, -1.95]
                pos2_list = [-0.15, -0.2, -0.25, -0.3, -0.35, -0.4, -0.45, -0.5, -0.55, -0.6, -0.65]
                pos3_list = [0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65]
                pos4_list = [0.45, 0.6, 0.75, 0.9, 1.05, 1.2, 1.35, 1.5, 1.65, 1.8, 1.95]
                toplu_liste = []
                for i in range(len(pos1_list)):
                    toplu_liste.append((pos1_list[i], pos2_list[i], pos3_list[i], pos4_list[i]))
        
                selected_list = random.choice(toplu_liste)
        
                pos1 = selected_list[0]
                pos2 = selected_list[1]
                pos3 = selected_list[2]
                pos4 = selected_list[3]
        
                if abs(pos2 - pos1) <= x:
                    return get_random_tuple()
        
                return x, pos1, pos2, pos3, pos4
        
            x, pos1, pos2, pos3, pos4 = get_random_tuple()
        elif shaping_5 == True:
            pos6 = 9999
            pos7 = 9999
            pos8 = 9999
            pos9 = 9999
            def get_random_tuple():
                width = np.arange(0.2, 0.6, 0.05)
                width_list = list(width)
        
                random.shuffle(width_list)
                x = float(f"{width_list[0]:.2f}")
        
                pos1_list = [-0.6, -0.8, -1, -1.2, -1.4, -1.6, -1.8, -2, -2.2, -2.4, -2.6]
                pos2_list = [-0.3, -0.4, -0.5, -0.6, -0.7, -0.8, -0.9, -1, -1.1, -1.2, -1.3]
                pos3_list = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
                pos4_list = [0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1, 1.2, 1.3]
                pos5_list = [0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2, 2.2, 2.4, 2.6]
        
                toplu_liste = []
                for i in range(len(pos1_list)):
                    toplu_liste.append((pos1_list[i], pos2_list[i], pos3_list[i], pos4_list[i], pos5_list[i]))
        
                selected_list = random.choice(toplu_liste)
        
                pos1 = float(f"{selected_list[0]:.2f}")
                pos2 = float(f"{selected_list[1]:.2f}")
                pos3 = float(f"{selected_list[2]:.2f}")
                pos4 = float(f"{selected_list[3]:.2f}")
                pos5 = float(f"{selected_list[4]:.2f}")
        
                if abs(pos2 - pos1) <= x:
                    return get_random_tuple()
        
                return x, pos1, pos2, pos3, pos4, pos5
        
            x, pos1, pos2, pos3, pos4, pos5 = get_random_tuple()
        elif shaping_6 == True:
            pos7 = 9999
            pos8 = 9999
            pos9 = 9999
            def get_random_tuple():
                width = np.arange(0.2, 0.6, 0.05)
                width_list = list(width)
        
                random.shuffle(width_list)
                x = float(f"{width_list[0]:.2f}")
        
                pos1_list = [-0.75, -1, -1.25, -1.5, -1.75, -2, -2.25, -2.5, -2.75, -3, -3.25]
                pos2_list = [-0.45, -0.6, -0.75, -0.9, -1.05, -1.2, -1.35, -1.5, -1.65, -1.8, -1.95]
                pos3_list = [-0.15, -0.2, -0.25, -0.3, -0.35, -0.4, -0.45, -0.5, -0.55, -0.6, -0.65]
                pos4_list = [0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65]
                pos5_list = [0.45, 0.6, 0.75, 0.9, 1.05, 1.2, 1.35, 1.5, 1.65, 1.8, 1.95]
                pos6_list = [0.75, 1, 1.25, 1.5, 1.75, 2, 2.25, 2.5, 2.75, 3, 3.25]
        
                toplu_liste = []
                for i in range(len(pos1_list)):
                    toplu_liste.append((pos1_list[i], pos2_list[i], pos3_list[i], pos4_list[i], pos5_list[i],pos6_list[i]))
        
                selected_list = random.choice(toplu_liste)
        
                pos1 = float(f"{selected_list[0]:.2f}")
                pos2 = float(f"{selected_list[1]:.2f}")
                pos3 = float(f"{selected_list[2]:.2f}")
                pos4 = float(f"{selected_list[3]:.2f}")
                pos5 = float(f"{selected_list[4]:.2f}")
                pos6 = float(f"{selected_list[5]:.2f}")
                
                if abs(pos2 - pos1) <= x:
                    return get_random_tuple()
        
                return x, pos1, pos2, pos3, pos4, pos5, pos6
        
            x, pos1, pos2, pos3, pos4, pos5, pos6 = get_random_tuple()
        elif shaping_7 == True:
            pos8 = 9999
            pos9 = 9999
            def get_random_tuple():
                width = np.arange(0.2, 0.6, 0.05)
                width_list = list(width)
        
                random.shuffle(width_list)
                x = float(f"{width_list[0]:.2f}")
        
                pos1_list = [-0.9, -1.2, -1.5, -1.8, -2.1, -2.4, -2.7, -3, -3.3, -3.6, -3.9]
                pos2_list = [-0.6, -0.8, -1, -1.2, -1.4, -1.6, -1.8, -2, -2.2, -2.4, -2.6]
                pos3_list = [-0.3, -0.4, -0.5, -0.6, -0.7, -0.8, -0.9, -1, -1.1, -1.2, -1.3]
                pos4_list = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
                pos5_list = [0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1, 1.2, 1.3]
                pos6_list = [0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2, 2.2, 2.4, 2.6]
                pos7_list = [0.9, 1.2, 1.5, 1.8, 2.1, 2.4, 2.7, 3, 3.3, 3.6, 3.9]
        
        
                toplu_liste = []
                for i in range(len(pos1_list)):
                    toplu_liste.append((pos1_list[i], pos2_list[i], pos3_list[i], pos4_list[i], pos5_list[i],pos6_list[i],pos7_list[i]))
        
                selected_list = random.choice(toplu_liste)
        
                pos1 = float(f"{selected_list[0]:.2f}")
                pos2 = float(f"{selected_list[1]:.2f}")
                pos3 = float(f"{selected_list[2]:.2f}")
                pos4 = float(f"{selected_list[3]:.2f}")
                pos5 = float(f"{selected_list[4]:.2f}")
                pos6 = float(f"{selected_list[5]:.2f}")
                pos7 = float(f"{selected_list[6]:.2f}")
                if abs(pos2 - pos1) <= x:
                    return get_random_tuple()
        
                return x, pos1, pos2, pos3, pos4, pos5, pos6, pos7
        
            x, pos1, pos2, pos3, pos4, pos5, pos6, pos7 = get_random_tuple()
        elif shaping_8 == True:
            pos9 = 9999
            def get_random_tuple():
                width = np.arange(0.2, 0.6, 0.05)
                width_list = list(width)
        
                random.shuffle(width_list)
                x = float(f"{width_list[0]:.2f}")
        
                pos1_list = [-1.2, -1.6, -2, -2.4, -2.8, -3.2, -3.6, -4, -4.4, -4.8, -5.2]
                pos2_list = [-0.9, -1.2, -1.5, -1.8, -2.1, -2.4, -2.7, -3, -3.3, -3.6, -3.9]
                pos3_list = [-0.6, -0.8, -1, -1.2, -1.4, -1.6, -1.8, -2, -2.2, -2.4, -2.6]
                pos4_list = [-0.3, -0.4, -0.5, -0.6, -0.7, -0.8, -0.9, -1, -1.1, -1.2, -1.3]
                pos5_list = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
                pos6_list = [0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1, 1.2, 1.3]
                pos7_list = [0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2, 2.2, 2.4, 2.6]
                pos8_list = [0.9, 1.2, 1.5, 1.8, 2.1, 2.4, 2.7, 3, 3.3, 3.6, 3.9]
                pos9_list = [1.2, 1.6, 2, 2.4, 2.8, 3.2, 3.6, 4, 4.4, 4.8, 5.2]
        
        
        
                toplu_liste = []
                for i in range(len(pos1_list)):
                    toplu_liste.append((pos1_list[i], pos2_list[i], pos3_list[i], pos4_list[i], pos5_list[i],pos6_list[i],pos7_list[i],pos8_list[i]))
        
                selected_list = random.choice(toplu_liste)
        
                pos1 = float(f"{selected_list[0]:.2f}")
                pos2 = float(f"{selected_list[1]:.2f}")
                pos3 = float(f"{selected_list[2]:.2f}")
                pos4 = float(f"{selected_list[3]:.2f}")
                pos5 = float(f"{selected_list[4]:.2f}")
                pos6 = float(f"{selected_list[5]:.2f}")
                pos7 = float(f"{selected_list[6]:.2f}")
                pos8 = float(f"{selected_list[7]:.2f}")
                if abs(pos2 - pos1) <= x:
                    return get_random_tuple()
        
                return x, pos1, pos2, pos3, pos4, pos5, pos6, pos7,pos8
        
            x, pos1, pos2, pos3, pos4, pos5, pos6, pos7,pos8 = get_random_tuple()
        elif shaping_9 == True:
            def get_random_tuple():
                width = np.arange(0.2, 0.6, 0.05)
                width_list = list(width)
        
                random.shuffle(width_list)
                x = float(f"{width_list[0]:.2f}")
        
                pos1_list = [-1.2, -1.6, -2, -2.4, -2.8, -3.2, -3.6, -4, -4.4, -4.8, -5.2]
                pos2_list = [-0.9, -1.2, -1.5, -1.8, -2.1, -2.4, -2.7, -3, -3.3, -3.6, -3.9]
                pos3_list = [-0.6, -0.8, -1, -1.2, -1.4, -1.6, -1.8, -2, -2.2, -2.4, -2.6]
                pos4_list = [-0.3, -0.4, -0.5, -0.6, -0.7, -0.8, -0.9, -1, -1.1, -1.2, -1.3]
                pos5_list = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
                pos6_list = [0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1, 1.2, 1.3]
                pos7_list = [0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2, 2.2, 2.4, 2.6]
                pos8_list = [0.9, 1.2, 1.5, 1.8, 2.1, 2.4, 2.7, 3, 3.3, 3.6, 3.9]
                pos9_list = [1.2, 1.6, 2, 2.4, 2.8, 3.2, 3.6, 4, 4.4, 4.8, 5.2]
        
        
                toplu_liste = []
                for i in range(len(pos1_list)):
                    toplu_liste.append((pos1_list[i], pos2_list[i], pos3_list[i], pos4_list[i], pos5_list[i],pos6_list[i],pos7_list[i],pos8_list[i],pos9_list[i]))
        
                selected_list = random.choice(toplu_liste)
        
                pos1 = float(f"{selected_list[0]:.2f}")
                pos2 = float(f"{selected_list[1]:.2f}")
                pos3 = float(f"{selected_list[2]:.2f}")
                pos4 = float(f"{selected_list[3]:.2f}")
                pos5 = float(f"{selected_list[4]:.2f}")
                pos6 = float(f"{selected_list[5]:.2f}")
                pos7 = float(f"{selected_list[6]:.2f}")
                pos8 = float(f"{selected_list[7]:.2f}")
                pos9 = float(f"{selected_list[8]:.2f}")
                if pos2 - pos1 <= x:
                    return get_random_tuple()
        
                return x, pos1, pos2, pos3, pos4, pos5, pos6, pos7,pos8,pos9
        
            x, pos1, pos2, pos3, pos4, pos5, pos6, pos7,pos8,pos9 = get_random_tuple()
            
        starting_space = round(abs(pos2-pos1),2)
        thisExp.addData("starting_width_deg", x)
        thisExp.addData("starting_space_deg", starting_space)
        next_5.keys = []
        next_5.rt = []
        _next_5_allKeys = []
        # keep track of which components have finished
        shapingComponents = [test_p1_5, test_p2_5, test_p3_5, test_p4_5, test_p5_5, test_p6, test_p7, test_p8, test_p9, next_5]
        for thisComponent in shapingComponents:
            thisComponent.tStart = None
            thisComponent.tStop = None
            thisComponent.tStartRefresh = None
            thisComponent.tStopRefresh = None
            if hasattr(thisComponent, 'status'):
                thisComponent.status = NOT_STARTED
        # reset timers
        t = 0
        _timeToFirstFrame = win.getFutureFlipTime(clock="now")
        frameN = -1
        
        # --- Run Routine "shaping" ---
        routineForceEnded = not continueRoutine
        while continueRoutine:
            # get current time
            t = routineTimer.getTime()
            tThisFlip = win.getFutureFlipTime(clock=routineTimer)
            tThisFlipGlobal = win.getFutureFlipTime(clock=None)
            frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
            # update/draw components on each frame
            # Run 'Each Frame' code from cd
            
            #adjusting the widths and coordinates
            if shaping_1 == True:
                for key in event.getKeys():
                    if key == thinner:
                            x -= 0.05 
                    elif key == thicker:
                            x += 0.05
            elif shaping_2 == True:
                for key in event.getKeys():
                    if key == thinner:
                            x -= 0.05  
                    elif key == thicker:
                            x += 0.05     
                    elif key == narrower:
                            pos1 += 0.05 
                            pos2 -= 0.05 
                    elif key == wider:
                            pos1 -= 0.05 
                            pos2 += 0.05 
            elif shaping_3 == True:
                for key in event.getKeys():
                    if key == thinner:
                            x -= 0.05 
                    elif key == thicker:
                            x += 0.05     
                    elif key == wider:
                            pos1 -= 0.05 
                            pos3 += 0.05 
                    elif key == narrower:
                            pos1 += 0.05 
                            pos3 -= 0.05 
            elif shaping_4 == True:
                for key in event.getKeys():
                    if key == thinner:
                            x -= 0.05 
                    elif key == thicker:
                            x += 0.05    
                    elif key == wider:
                            pos1 -= 0.15
                            pos2 -= 0.05
                            pos3 += 0.05
                            pos4 += 0.15
                    elif key == narrower:
                            pos1 += 0.15
                            pos2 += 0.05
                            pos3 -= 0.05
                            pos4 -= 0.15
            elif shaping_5 == True:
                for key in event.getKeys():
                    if key == thinner:
                            x -= 0.05 
                    elif key == thicker:
                            x += 0.05  
                    elif key == wider:
                            pos1 -= 0.1
                            pos2 -= 0.05
                            pos4 += 0.05
                            pos5 += 0.1
                    elif key == narrower:
                            pos1 += 0.1
                            pos2 += 0.05
                            pos4 -= 0.05
                            pos5 -= 0.1
            elif shaping_6 == True:
                for key in event.getKeys():
                    if key == thinner:
                            x -= 0.05 
                    elif key == thicker:
                            x += 0.05    
                    elif key == wider:
                            pos1 -= 0.25
                            pos2 -= 0.15
                            pos3 -= 0.05
                            pos4 += 0.05
                            pos5 += 0.15
                            pos6 += 0.25
                    elif key == narrower:
                            pos1 += 0.25
                            pos2 += 0.15
                            pos3 += 0.05
                            pos4 -= 0.05
                            pos5 -= 0.15
                            pos6 -= 0.25
            elif shaping_7 == True:
                for key in event.getKeys():
                    if key == thinner:
                            x -= 0.05 
                    elif key == thicker:
                            x += 0.05  
                    elif key == wider:
                            pos1 -= 0.15
                            pos2 -= 0.1
                            pos3 -= 0.05
                            pos5 += 0.05
                            pos6 += 0.1
                            pos7 += 0.15
                    elif key == narrower:
                            pos1 += 0.15
                            pos2 += 0.1
                            pos3 += 0.05
                            pos5 -= 0.05
                            pos6 -= 0.1
                            pos7 -= 0.15
            elif shaping_8 == True:
                for key in event.getKeys():
                    if key == thinner:
                            x -= 0.05 
                    elif key == thicker:
                            x += 0.05    
                    elif key == wider:
                            pos1 -= 0.35
                            pos2 -= 0.25
                            pos3 -= 0.15
                            pos4 -= 0.05
                            pos5 += 0.05
                            pos6 += 0.15
                            pos7 += 0.25
                            pos8 += 0.35 
                    elif key == narrower:
                            pos1 += 0.35
                            pos2 += 0.25
                            pos3 += 0.15
                            pos4 += 0.05
                            pos5 -= 0.05
                            pos6 -= 0.15
                            pos7 -= 0.25
                            pos8 -= 0.35  
            elif shaping_9 == True:
                for key in event.getKeys():
                    if key == thinner:
                            x -= 0.05 
                    elif key == thicker:
                            x += 0.05  
                    elif key == wider:
                            pos1 -= 0.2
                            pos2 -= 0.15
                            pos3 -= 0.1
                            pos4 -= 0.05
                            pos6 += 0.05
                            pos7 += 0.1
                            pos8 += 0.15
                            pos9 += 0.2
                    elif key == narrower:
                            pos1 += 0.2
                            pos2 += 0.15
                            pos3 += 0.1
                            pos4 += 0.05
                            pos6 -= 0.05
                            pos7 -= 0.1
                            pos8 -= 0.15
                            pos9 -= 0.2
            
            # *test_p1_5* updates
            
            # if test_p1_5 is starting this frame...
            if test_p1_5.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                test_p1_5.frameNStart = frameN  # exact frame index
                test_p1_5.tStart = t  # local t and not account for scr refresh
                test_p1_5.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(test_p1_5, 'tStartRefresh')  # time at next scr refresh
                # update status
                test_p1_5.status = STARTED
                test_p1_5.setAutoDraw(True)
            
            # if test_p1_5 is active this frame...
            if test_p1_5.status == STARTED:
                # update params
                test_p1_5.setFillColor(renk_p1, log=False)
                test_p1_5.setPos((pos1, 0), log=False)
                test_p1_5.setSize((x, 1.9), log=False)
                test_p1_5.setLineColor(renk_p1, log=False)
            
            # *test_p2_5* updates
            
            # if test_p2_5 is starting this frame...
            if test_p2_5.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                test_p2_5.frameNStart = frameN  # exact frame index
                test_p2_5.tStart = t  # local t and not account for scr refresh
                test_p2_5.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(test_p2_5, 'tStartRefresh')  # time at next scr refresh
                # update status
                test_p2_5.status = STARTED
                test_p2_5.setAutoDraw(True)
            
            # if test_p2_5 is active this frame...
            if test_p2_5.status == STARTED:
                # update params
                test_p2_5.setFillColor(renk_p2, log=False)
                test_p2_5.setPos((pos2, 0), log=False)
                test_p2_5.setSize((x, 1.9), log=False)
                test_p2_5.setLineColor(renk_p2, log=False)
            
            # *test_p3_5* updates
            
            # if test_p3_5 is starting this frame...
            if test_p3_5.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                test_p3_5.frameNStart = frameN  # exact frame index
                test_p3_5.tStart = t  # local t and not account for scr refresh
                test_p3_5.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(test_p3_5, 'tStartRefresh')  # time at next scr refresh
                # update status
                test_p3_5.status = STARTED
                test_p3_5.setAutoDraw(True)
            
            # if test_p3_5 is active this frame...
            if test_p3_5.status == STARTED:
                # update params
                test_p3_5.setFillColor(renk_p3, log=False)
                test_p3_5.setPos((pos3, 0), log=False)
                test_p3_5.setSize((x, 1.9), log=False)
                test_p3_5.setLineColor(renk_p3, log=False)
            
            # *test_p4_5* updates
            
            # if test_p4_5 is starting this frame...
            if test_p4_5.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                test_p4_5.frameNStart = frameN  # exact frame index
                test_p4_5.tStart = t  # local t and not account for scr refresh
                test_p4_5.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(test_p4_5, 'tStartRefresh')  # time at next scr refresh
                # update status
                test_p4_5.status = STARTED
                test_p4_5.setAutoDraw(True)
            
            # if test_p4_5 is active this frame...
            if test_p4_5.status == STARTED:
                # update params
                test_p4_5.setFillColor(renk_p4, log=False)
                test_p4_5.setPos((pos4, 0), log=False)
                test_p4_5.setSize((x, 1.9), log=False)
                test_p4_5.setLineColor(renk_p4, log=False)
            
            # *test_p5_5* updates
            
            # if test_p5_5 is starting this frame...
            if test_p5_5.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                test_p5_5.frameNStart = frameN  # exact frame index
                test_p5_5.tStart = t  # local t and not account for scr refresh
                test_p5_5.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(test_p5_5, 'tStartRefresh')  # time at next scr refresh
                # update status
                test_p5_5.status = STARTED
                test_p5_5.setAutoDraw(True)
            
            # if test_p5_5 is active this frame...
            if test_p5_5.status == STARTED:
                # update params
                test_p5_5.setFillColor(renk_p5, log=False)
                test_p5_5.setPos((pos5, 0), log=False)
                test_p5_5.setSize((x, 1.9), log=False)
                test_p5_5.setLineColor(renk_p5, log=False)
            
            # *test_p6* updates
            
            # if test_p6 is starting this frame...
            if test_p6.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                test_p6.frameNStart = frameN  # exact frame index
                test_p6.tStart = t  # local t and not account for scr refresh
                test_p6.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(test_p6, 'tStartRefresh')  # time at next scr refresh
                # update status
                test_p6.status = STARTED
                test_p6.setAutoDraw(True)
            
            # if test_p6 is active this frame...
            if test_p6.status == STARTED:
                # update params
                test_p6.setFillColor(renk_p5, log=False)
                test_p6.setPos((pos6, 0), log=False)
                test_p6.setSize((x, 1.9), log=False)
                test_p6.setLineColor(renk_p5, log=False)
            
            # *test_p7* updates
            
            # if test_p7 is starting this frame...
            if test_p7.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                test_p7.frameNStart = frameN  # exact frame index
                test_p7.tStart = t  # local t and not account for scr refresh
                test_p7.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(test_p7, 'tStartRefresh')  # time at next scr refresh
                # update status
                test_p7.status = STARTED
                test_p7.setAutoDraw(True)
            
            # if test_p7 is active this frame...
            if test_p7.status == STARTED:
                # update params
                test_p7.setFillColor(renk_p5, log=False)
                test_p7.setPos((pos7, 0), log=False)
                test_p7.setSize((x, 1.9), log=False)
                test_p7.setLineColor(renk_p5, log=False)
            
            # *test_p8* updates
            
            # if test_p8 is starting this frame...
            if test_p8.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                test_p8.frameNStart = frameN  # exact frame index
                test_p8.tStart = t  # local t and not account for scr refresh
                test_p8.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(test_p8, 'tStartRefresh')  # time at next scr refresh
                # update status
                test_p8.status = STARTED
                test_p8.setAutoDraw(True)
            
            # if test_p8 is active this frame...
            if test_p8.status == STARTED:
                # update params
                test_p8.setFillColor(renk_p5, log=False)
                test_p8.setPos((pos8, 0), log=False)
                test_p8.setSize((x, 1.9), log=False)
                test_p8.setLineColor(renk_p5, log=False)
            
            # *test_p9* updates
            
            # if test_p9 is starting this frame...
            if test_p9.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                test_p9.frameNStart = frameN  # exact frame index
                test_p9.tStart = t  # local t and not account for scr refresh
                test_p9.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(test_p9, 'tStartRefresh')  # time at next scr refresh
                # update status
                test_p9.status = STARTED
                test_p9.setAutoDraw(True)
            
            # if test_p9 is active this frame...
            if test_p9.status == STARTED:
                # update params
                test_p9.setFillColor(renk_p5, log=False)
                test_p9.setPos((pos9, 0), log=False)
                test_p9.setSize((x, 1.9), log=False)
                test_p9.setLineColor(renk_p5, log=False)
            
            # *next_5* updates
            waitOnFlip = False
            
            # if next_5 is starting this frame...
            if next_5.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                next_5.frameNStart = frameN  # exact frame index
                next_5.tStart = t  # local t and not account for scr refresh
                next_5.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(next_5, 'tStartRefresh')  # time at next scr refresh
                # update status
                next_5.status = STARTED
                # keyboard checking is just starting
                waitOnFlip = True
                win.callOnFlip(next_5.clock.reset)  # t=0 on next screen flip
                win.callOnFlip(next_5.clearEvents, eventType='keyboard')  # clear events on next screen flip
            if next_5.status == STARTED and not waitOnFlip:
                theseKeys = next_5.getKeys(keyList=['return'], waitRelease=False)
                _next_5_allKeys.extend(theseKeys)
                if len(_next_5_allKeys):
                    next_5.keys = _next_5_allKeys[-1].name  # just the last key pressed
                    next_5.rt = _next_5_allKeys[-1].rt
                    # a response ends the routine
                    continueRoutine = False
            
            # check for quit (typically the Esc key)
            if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
                core.quit()
            
            # check if all components have finished
            if not continueRoutine:  # a component has requested a forced-end of Routine
                routineForceEnded = True
                break
            continueRoutine = False  # will revert to True if at least one component still running
            for thisComponent in shapingComponents:
                if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                    continueRoutine = True
                    break  # at least one component has not yet finished
            
            # refresh the screen
            if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                win.flip()
        
        # --- Ending Routine "shaping" ---
        for thisComponent in shapingComponents:
            if hasattr(thisComponent, "setAutoDraw"):
                thisComponent.setAutoDraw(False)
        # Run 'End Routine' code from cd
        #spacing variable
        space_deg = abs(round((pos2 - pos1), 2))
        #adding to the data
        thisExp.addData("response_width_degree",x)
        thisExp.addData("response_spacing_degree",space_deg)
        
        #converting degrees to pixels
        x_pixel = round(deg2pix(x, monitor = win.monitor), 5)
        space_pixel = round(deg2pix(space_deg, monitor = win.monitor), 5)
        #adding to the data
        thisExp.addData("response_width_pixel",x_pixel)
        thisExp.addData("response_spacing_pixel",space_pixel)
        
        
        # check responses
        if next_5.keys in ['', [], None]:  # No response was made
            next_5.keys = None
        loop.addData('next_5.keys',next_5.keys)
        if next_5.keys != None:  # we had a response
            loop.addData('next_5.rt', next_5.rt)
        # the Routine "shaping" was not non-slip safe, so reset the non-slip timer
        routineTimer.reset()
        thisExp.nextEntry()
        
    # completed 1.0 repeats of 'loop'
    
    
    # --- Prepare to start Routine "break_2" ---
    continueRoutine = True
    # update component parameters for each repeat
    text_4.setText(str(blocks.thisN+ 1) + '/6 done.' )
    key_resp_2.keys = []
    key_resp_2.rt = []
    _key_resp_2_allKeys = []
    # keep track of which components have finished
    break_2Components = [text_3, text_4, key_resp_2]
    for thisComponent in break_2Components:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    frameN = -1
    
    # --- Run Routine "break_2" ---
    routineForceEnded = not continueRoutine
    while continueRoutine:
        # get current time
        t = routineTimer.getTime()
        tThisFlip = win.getFutureFlipTime(clock=routineTimer)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *text_3* updates
        
        # if text_3 is starting this frame...
        if text_3.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            text_3.frameNStart = frameN  # exact frame index
            text_3.tStart = t  # local t and not account for scr refresh
            text_3.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_3, 'tStartRefresh')  # time at next scr refresh
            # add timestamp to datafile
            thisExp.timestampOnFlip(win, 'text_3.started')
            # update status
            text_3.status = STARTED
            text_3.setAutoDraw(True)
        
        # if text_3 is active this frame...
        if text_3.status == STARTED:
            # update params
            pass
        
        # *text_4* updates
        
        # if text_4 is starting this frame...
        if text_4.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            text_4.frameNStart = frameN  # exact frame index
            text_4.tStart = t  # local t and not account for scr refresh
            text_4.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_4, 'tStartRefresh')  # time at next scr refresh
            # add timestamp to datafile
            thisExp.timestampOnFlip(win, 'text_4.started')
            # update status
            text_4.status = STARTED
            text_4.setAutoDraw(True)
        
        # if text_4 is active this frame...
        if text_4.status == STARTED:
            # update params
            pass
        
        # *key_resp_2* updates
        waitOnFlip = False
        
        # if key_resp_2 is starting this frame...
        if key_resp_2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            key_resp_2.frameNStart = frameN  # exact frame index
            key_resp_2.tStart = t  # local t and not account for scr refresh
            key_resp_2.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(key_resp_2, 'tStartRefresh')  # time at next scr refresh
            # add timestamp to datafile
            thisExp.timestampOnFlip(win, 'key_resp_2.started')
            # update status
            key_resp_2.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(key_resp_2.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(key_resp_2.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if key_resp_2.status == STARTED and not waitOnFlip:
            theseKeys = key_resp_2.getKeys(keyList=['c'], waitRelease=False)
            _key_resp_2_allKeys.extend(theseKeys)
            if len(_key_resp_2_allKeys):
                key_resp_2.keys = _key_resp_2_allKeys[-1].name  # just the last key pressed
                key_resp_2.rt = _key_resp_2_allKeys[-1].rt
                # a response ends the routine
                continueRoutine = False
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            routineForceEnded = True
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in break_2Components:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # --- Ending Routine "break_2" ---
    for thisComponent in break_2Components:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # the Routine "break_2" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    thisExp.nextEntry()
    
# completed 6.0 repeats of 'blocks'
# --- Prepare to start Routine "info" ---
continueRoutine = True
# update component parameters for each repeat
key_resp_2.keys = []
key_resp_2.rt = []
_key_resp_2_allKeys = []
# keep track of which components have finished
infoComponents = [text_2, key_resp_2]
for thisComponent in infoComponents:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
frameN = -1

# --- Run Routine "info" ---
routineForceEnded = not continueRoutine
while continueRoutine:
    # get current time
    t = routineTimer.getTime()
    tThisFlip = win.getFutureFlipTime(clock=routineTimer)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *text_2* updates
    
    # if text_2 is starting this frame...
    if text_2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        text_2.frameNStart = frameN  # exact frame index
        text_2.tStart = t  # local t and not account for scr refresh
        text_2.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(text_2, 'tStartRefresh')  # time at next scr refresh
        # add timestamp to datafile
        thisExp.timestampOnFlip(win, 'text_2.started')
        # update status
        text_2.status = STARTED
        text_2.setAutoDraw(True)
    
    # if text_2 is active this frame...
    if text_2.status == STARTED:
        # update params
        pass
    
    # *key_resp_2* updates
    waitOnFlip = False
    
    # if key_resp_2 is starting this frame...
    if key_resp_2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        key_resp_2.frameNStart = frameN  # exact frame index
        key_resp_2.tStart = t  # local t and not account for scr refresh
        key_resp_2.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(key_resp_2, 'tStartRefresh')  # time at next scr refresh
        # update status
        key_resp_2.status = STARTED
        # keyboard checking is just starting
        waitOnFlip = True
        win.callOnFlip(key_resp_2.clock.reset)  # t=0 on next screen flip
        win.callOnFlip(key_resp_2.clearEvents, eventType='keyboard')  # clear events on next screen flip
    if key_resp_2.status == STARTED and not waitOnFlip:
        theseKeys = key_resp_2.getKeys(keyList=["c"], waitRelease=False)
        _key_resp_2_allKeys.extend(theseKeys)
        if len(_key_resp_2_allKeys):
            key_resp_2.keys = _key_resp_2_allKeys[-1].name  # just the last key pressed
            key_resp_2.rt = _key_resp_2_allKeys[-1].rt
            # a response ends the routine
            continueRoutine = False
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
        if eyetracker:
            eyetracker.setConnectionState(False)
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        routineForceEnded = True
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in infoComponents:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# --- Ending Routine "info" ---
for thisComponent in infoComponents:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
# check responses
if key_resp_2.keys in ['', [], None]:  # No response was made
    key_resp_2.keys = None
thisExp.addData('key_resp_2.keys',key_resp_2.keys)
if key_resp_2.keys != None:  # we had a response
    thisExp.addData('key_resp_2.rt', key_resp_2.rt)
thisExp.nextEntry()
# the Routine "info" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# set up handler to look after randomisation of conditions etc
first = data.TrialHandler(nReps=1.0, method='random', 
    extraInfo=expInfo, originPath=-1,
    trialList=data.importConditions('control.xlsx'),
    seed=None, name='first')
thisExp.addLoop(first)  # add the loop to the experiment
thisFirst = first.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb = thisFirst.rgb)
if thisFirst != None:
    for paramName in thisFirst:
        exec('{} = thisFirst[paramName]'.format(paramName))

for thisFirst in first:
    currentLoop = first
    # abbreviate parameter names if possible (e.g. rgb = thisFirst.rgb)
    if thisFirst != None:
        for paramName in thisFirst:
            exec('{} = thisFirst[paramName]'.format(paramName))
    
    # --- Prepare to start Routine "wait" ---
    continueRoutine = True
    # update component parameters for each repeat
    # Run 'Begin Routine' code from code_4
    
    random_n = [1,1.25,1.5,1.75,2]
    shuffle(random_n)
    
    # keep track of which components have finished
    waitComponents = [fixation_4]
    for thisComponent in waitComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    frameN = -1
    
    # --- Run Routine "wait" ---
    routineForceEnded = not continueRoutine
    while continueRoutine:
        # get current time
        t = routineTimer.getTime()
        tThisFlip = win.getFutureFlipTime(clock=routineTimer)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *fixation_4* updates
        
        # if fixation_4 is starting this frame...
        if fixation_4.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            fixation_4.frameNStart = frameN  # exact frame index
            fixation_4.tStart = t  # local t and not account for scr refresh
            fixation_4.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(fixation_4, 'tStartRefresh')  # time at next scr refresh
            # update status
            fixation_4.status = STARTED
            fixation_4.setAutoDraw(True)
        
        # if fixation_4 is active this frame...
        if fixation_4.status == STARTED:
            # update params
            pass
        
        # if fixation_4 is stopping this frame...
        if fixation_4.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > fixation_4.tStartRefresh + random_n[0]-frameTolerance:
                # keep track of stop time/frame for later
                fixation_4.tStop = t  # not accounting for scr refresh
                fixation_4.frameNStop = frameN  # exact frame index
                # update status
                fixation_4.status = FINISHED
                fixation_4.setAutoDraw(False)
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
            if eyetracker:
                eyetracker.setConnectionState(False)
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            routineForceEnded = True
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in waitComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # --- Ending Routine "wait" ---
    for thisComponent in waitComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # the Routine "wait" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # --- Prepare to start Routine "present" ---
    continueRoutine = True
    # update component parameters for each repeat
    # Run 'Begin Routine' code from code_2
    x = random.random()
    
    if x < 0.5:
        posx = trans_posi(posx)
    stimuli_2.setPos((posx, 0))
    stimuli_2.setSize((w, 1.9))
    # keep track of which components have finished
    presentComponents = [stimuli_2, fixation]
    for thisComponent in presentComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    frameN = -1
    
    # --- Run Routine "present" ---
    routineForceEnded = not continueRoutine
    while continueRoutine:
        # get current time
        t = routineTimer.getTime()
        tThisFlip = win.getFutureFlipTime(clock=routineTimer)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *stimuli_2* updates
        
        # if stimuli_2 is starting this frame...
        if stimuli_2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            stimuli_2.frameNStart = frameN  # exact frame index
            stimuli_2.tStart = t  # local t and not account for scr refresh
            stimuli_2.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(stimuli_2, 'tStartRefresh')  # time at next scr refresh
            # update status
            stimuli_2.status = STARTED
            stimuli_2.setAutoDraw(True)
        
        # if stimuli_2 is active this frame...
        if stimuli_2.status == STARTED:
            # update params
            pass
        
        # if stimuli_2 is stopping this frame...
        if stimuli_2.status == STARTED:
            if frameN >= (stimuli_2.frameNStart + 9):
                # keep track of stop time/frame for later
                stimuli_2.tStop = t  # not accounting for scr refresh
                stimuli_2.frameNStop = frameN  # exact frame index
                # update status
                stimuli_2.status = FINISHED
                stimuli_2.setAutoDraw(False)
        
        # *fixation* updates
        
        # if fixation is starting this frame...
        if fixation.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            fixation.frameNStart = frameN  # exact frame index
            fixation.tStart = t  # local t and not account for scr refresh
            fixation.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(fixation, 'tStartRefresh')  # time at next scr refresh
            # update status
            fixation.status = STARTED
            fixation.setAutoDraw(True)
        
        # if fixation is active this frame...
        if fixation.status == STARTED:
            # update params
            pass
        
        # if fixation is stopping this frame...
        if fixation.status == STARTED:
            if frameN >= (fixation.frameNStart + 9):
                # keep track of stop time/frame for later
                fixation.tStop = t  # not accounting for scr refresh
                fixation.frameNStop = frameN  # exact frame index
                # update status
                fixation.status = FINISHED
                fixation.setAutoDraw(False)
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
            if eyetracker:
                eyetracker.setConnectionState(False)
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            routineForceEnded = True
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in presentComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # --- Ending Routine "present" ---
    for thisComponent in presentComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # the Routine "present" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # --- Prepare to start Routine "resp" ---
    continueRoutine = True
    # update component parameters for each repeat
    # Run 'Begin Routine' code from code
    
        
    
    #randomize the widths
    width = np.arange(0.1, 0.5, 0.05)
    width_list = list(width)
    
    random.shuffle(width_list)
    x = float(f"{width_list[0]:.2f}")
    
    thisExp.addData("starting_width_deg", x)
    next.keys = []
    next.rt = []
    _next_allKeys = []
    # keep track of which components have finished
    respComponents = [adjusting, next]
    for thisComponent in respComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    frameN = -1
    
    # --- Run Routine "resp" ---
    routineForceEnded = not continueRoutine
    while continueRoutine:
        # get current time
        t = routineTimer.getTime()
        tThisFlip = win.getFutureFlipTime(clock=routineTimer)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        # Run 'Each Frame' code from code
        for key in event.getKeys():
            if key == 'left':
                x -= 0.05 
            elif key == 'right':
                x += 0.05
        
        # *adjusting* updates
        
        # if adjusting is starting this frame...
        if adjusting.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            adjusting.frameNStart = frameN  # exact frame index
            adjusting.tStart = t  # local t and not account for scr refresh
            adjusting.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(adjusting, 'tStartRefresh')  # time at next scr refresh
            # add timestamp to datafile
            thisExp.timestampOnFlip(win, 'adjusting.started')
            # update status
            adjusting.status = STARTED
            adjusting.setAutoDraw(True)
        
        # if adjusting is active this frame...
        if adjusting.status == STARTED:
            # update params
            adjusting.setSize((x, 1.9), log=False)
        
        # *next* updates
        waitOnFlip = False
        
        # if next is starting this frame...
        if next.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            next.frameNStart = frameN  # exact frame index
            next.tStart = t  # local t and not account for scr refresh
            next.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(next, 'tStartRefresh')  # time at next scr refresh
            # update status
            next.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(next.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(next.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if next.status == STARTED and not waitOnFlip:
            theseKeys = next.getKeys(keyList=["return"], waitRelease=False)
            _next_allKeys.extend(theseKeys)
            if len(_next_allKeys):
                next.keys = _next_allKeys[-1].name  # just the last key pressed
                next.rt = _next_allKeys[-1].rt
                # a response ends the routine
                continueRoutine = False
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
            if eyetracker:
                eyetracker.setConnectionState(False)
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            routineForceEnded = True
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in respComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # --- Ending Routine "resp" ---
    for thisComponent in respComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # Run 'End Routine' code from code
    thisExp.addData("response_width_degree",x)
    x_pixel = round(deg2pix(x, monitor = win.monitor), 5)
    
    thisExp.addData("response_width_pixel",x_pixel)
    # check responses
    if next.keys in ['', [], None]:  # No response was made
        next.keys = None
    first.addData('next.keys',next.keys)
    if next.keys != None:  # we had a response
        first.addData('next.rt', next.rt)
    # the Routine "resp" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    thisExp.nextEntry()
    
# completed 6.0 repeats of 'first'


# --- Prepare to start Routine "info_2" ---
continueRoutine = True
# update component parameters for each repeat
key_resp_3.keys = []
key_resp_3.rt = []
_key_resp_3_allKeys = []
# keep track of which components have finished
info_2Components = [text_control2, key_resp_3]
for thisComponent in info_2Components:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
frameN = -1

# --- Run Routine "info_2" ---
routineForceEnded = not continueRoutine
while continueRoutine:
    # get current time
    t = routineTimer.getTime()
    tThisFlip = win.getFutureFlipTime(clock=routineTimer)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *text_control2* updates
    
    # if text_control2 is starting this frame...
    if text_control2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        text_control2.frameNStart = frameN  # exact frame index
        text_control2.tStart = t  # local t and not account for scr refresh
        text_control2.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(text_control2, 'tStartRefresh')  # time at next scr refresh
        # add timestamp to datafile
        thisExp.timestampOnFlip(win, 'text_control2.started')
        # update status
        text_control2.status = STARTED
        text_control2.setAutoDraw(True)
    
    # if text_3 is active this frame...
    if text_control2.status == STARTED:
        # update params
        pass
    
    # *key_resp_3* updates
    waitOnFlip = False
    
    # if key_resp_3 is starting this frame...
    if key_resp_3.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        key_resp_3.frameNStart = frameN  # exact frame index
        key_resp_3.tStart = t  # local t and not account for scr refresh
        key_resp_3.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(key_resp_3, 'tStartRefresh')  # time at next scr refresh
        # update status
        key_resp_3.status = STARTED
        # keyboard checking is just starting
        waitOnFlip = True
        win.callOnFlip(key_resp_3.clock.reset)  # t=0 on next screen flip
        win.callOnFlip(key_resp_3.clearEvents, eventType='keyboard')  # clear events on next screen flip
    if key_resp_3.status == STARTED and not waitOnFlip:
        theseKeys = key_resp_3.getKeys(keyList=["c"], waitRelease=False)
        _key_resp_3_allKeys.extend(theseKeys)
        if len(_key_resp_3_allKeys):
            key_resp_3.keys = _key_resp_3_allKeys[-1].name  # just the last key pressed
            key_resp_3.rt = _key_resp_3_allKeys[-1].rt
            # a response ends the routine
            continueRoutine = False
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
        if eyetracker:
            eyetracker.setConnectionState(False)
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        routineForceEnded = True
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in info_2Components:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# --- Ending Routine "info_2" ---
for thisComponent in info_2Components:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
# check responses
if key_resp_3.keys in ['', [], None]:  # No response was made
    key_resp_3.keys = None
thisExp.addData('key_resp_3.keys',key_resp_3.keys)
if key_resp_3.keys != None:  # we had a response
    thisExp.addData('key_resp_3.rt', key_resp_3.rt)
thisExp.nextEntry()
# the Routine "info_2" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# set up handler to look after randomisation of conditions etc
second = data.TrialHandler(nReps=1.0, method='random', 
    extraInfo=expInfo, originPath=-1,
    trialList=data.importConditions('control_2.xlsx'),
    seed=None, name='second')
thisExp.addLoop(second)  # add the loop to the experiment
thisSecond = second.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb = thisSecond.rgb)
if thisSecond != None:
    for paramName in thisSecond:
        exec('{} = thisSecond[paramName]'.format(paramName))

for thisSecond in second:
    currentLoop = second
    # abbreviate parameter names if possible (e.g. rgb = thisSecond.rgb)
    if thisSecond != None:
        for paramName in thisSecond:
            exec('{} = thisSecond[paramName]'.format(paramName))
    
    # --- Prepare to start Routine "wait" ---
    continueRoutine = True
    # update component parameters for each repeat
    # Run 'Begin Routine' code from code_4
    
    random_n = [1,1.25,1.5,1.75,2]
    shuffle(random_n)
    
    # keep track of which components have finished
    waitComponents = [fixation_4]
    for thisComponent in waitComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    frameN = -1
    
    # --- Run Routine "wait" ---
    routineForceEnded = not continueRoutine
    while continueRoutine:
        # get current time
        t = routineTimer.getTime()
        tThisFlip = win.getFutureFlipTime(clock=routineTimer)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *fixation_4* updates
        
        # if fixation_4 is starting this frame...
        if fixation_4.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            fixation_4.frameNStart = frameN  # exact frame index
            fixation_4.tStart = t  # local t and not account for scr refresh
            fixation_4.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(fixation_4, 'tStartRefresh')  # time at next scr refresh
            # update status
            fixation_4.status = STARTED
            fixation_4.setAutoDraw(True)
        
        # if fixation_4 is active this frame...
        if fixation_4.status == STARTED:
            # update params
            pass
        
        # if fixation_4 is stopping this frame...
        if fixation_4.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > fixation_4.tStartRefresh + random_n[0]-frameTolerance:
                # keep track of stop time/frame for later
                fixation_4.tStop = t  # not accounting for scr refresh
                fixation_4.frameNStop = frameN  # exact frame index
                # update status
                fixation_4.status = FINISHED
                fixation_4.setAutoDraw(False)
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
            if eyetracker:
                eyetracker.setConnectionState(False)
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            routineForceEnded = True
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in waitComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # --- Ending Routine "wait" ---
    for thisComponent in waitComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # the Routine "wait" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # --- Prepare to start Routine "present_2" ---
    continueRoutine = True
    # update component parameters for each repeat
    # Run 'Begin Routine' code from code_3
    x = random.random()
    
    
    
    if x < 0.5:
        posx1 = trans_posi(posx1)
        posx2 = trans_posi(posx2)
    stimuli1.setPos((posx1, 0))
    stimuli1.setSize((w, 1.9))
    stimuli2.setPos((posx2, 0))
    stimuli2.setSize((w, 1.9))
    # keep track of which components have finished
    present_2Components = [stimuli1, stimuli2, fixation_2]
    for thisComponent in present_2Components:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    frameN = -1
    
    # --- Run Routine "present_2" ---
    routineForceEnded = not continueRoutine
    while continueRoutine:
        # get current time
        t = routineTimer.getTime()
        tThisFlip = win.getFutureFlipTime(clock=routineTimer)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *stimuli1* updates
        
        # if stimuli1 is starting this frame...
        if stimuli1.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            stimuli1.frameNStart = frameN  # exact frame index
            stimuli1.tStart = t  # local t and not account for scr refresh
            stimuli1.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(stimuli1, 'tStartRefresh')  # time at next scr refresh
            # update status
            stimuli1.status = STARTED
            stimuli1.setAutoDraw(True)
        
        # if stimuli1 is active this frame...
        if stimuli1.status == STARTED:
            # update params
            pass
        
        # if stimuli1 is stopping this frame...
        if stimuli1.status == STARTED:
            if frameN >= (stimuli1.frameNStart + 9):
                # keep track of stop time/frame for later
                stimuli1.tStop = t  # not accounting for scr refresh
                stimuli1.frameNStop = frameN  # exact frame index
                # update status
                stimuli1.status = FINISHED
                stimuli1.setAutoDraw(False)
        
        # *stimuli2* updates
        
        # if stimuli2 is starting this frame...
        if stimuli2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            stimuli2.frameNStart = frameN  # exact frame index
            stimuli2.tStart = t  # local t and not account for scr refresh
            stimuli2.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(stimuli2, 'tStartRefresh')  # time at next scr refresh
            # update status
            stimuli2.status = STARTED
            stimuli2.setAutoDraw(True)
        
        # if stimuli2 is active this frame...
        if stimuli2.status == STARTED:
            # update params
            pass
        
        # if stimuli2 is stopping this frame...
        if stimuli2.status == STARTED:
            if frameN >= (stimuli2.frameNStart + 9):
                # keep track of stop time/frame for later
                stimuli2.tStop = t  # not accounting for scr refresh
                stimuli2.frameNStop = frameN  # exact frame index
                # update status
                stimuli2.status = FINISHED
                stimuli2.setAutoDraw(False)
        
        # *fixation_2* updates
        
        # if fixation_2 is starting this frame...
        if fixation_2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            fixation_2.frameNStart = frameN  # exact frame index
            fixation_2.tStart = t  # local t and not account for scr refresh
            fixation_2.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(fixation_2, 'tStartRefresh')  # time at next scr refresh
            # update status
            fixation_2.status = STARTED
            fixation_2.setAutoDraw(True)
        
        # if fixation_2 is active this frame...
        if fixation_2.status == STARTED:
            # update params
            pass
        
        # if fixation_2 is stopping this frame...
        if fixation_2.status == STARTED:
            if frameN >= (fixation_2.frameNStart + 9):
                # keep track of stop time/frame for later
                fixation_2.tStop = t  # not accounting for scr refresh
                fixation_2.frameNStop = frameN  # exact frame index
                # update status
                fixation_2.status = FINISHED
                fixation_2.setAutoDraw(False)
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
            if eyetracker:
                eyetracker.setConnectionState(False)
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            routineForceEnded = True
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in present_2Components:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # --- Ending Routine "present_2" ---
    for thisComponent in present_2Components:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # the Routine "present_2" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # --- Prepare to start Routine "resp_2" ---
    continueRoutine = True
    # update component parameters for each repeat
    key_resp.keys = []
    key_resp.rt = []
    _key_resp_allKeys = []
    # keep track of which components have finished
    resp_2Components = [fixation_3, key_resp]
    for thisComponent in resp_2Components:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    frameN = -1
    
    # --- Run Routine "resp_2" ---
    routineForceEnded = not continueRoutine
    while continueRoutine:
        # get current time
        t = routineTimer.getTime()
        tThisFlip = win.getFutureFlipTime(clock=routineTimer)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *fixation_3* updates
        
        # if fixation_3 is starting this frame...
        if fixation_3.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            fixation_3.frameNStart = frameN  # exact frame index
            fixation_3.tStart = t  # local t and not account for scr refresh
            fixation_3.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(fixation_3, 'tStartRefresh')  # time at next scr refresh
            # update status
            fixation_3.status = STARTED
            fixation_3.setAutoDraw(True)
        
        # if fixation_3 is active this frame...
        if fixation_3.status == STARTED:
            # update params
            pass
        
        # *key_resp* updates
        waitOnFlip = False
        
        # if key_resp is starting this frame...
        if key_resp.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            key_resp.frameNStart = frameN  # exact frame index
            key_resp.tStart = t  # local t and not account for scr refresh
            key_resp.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(key_resp, 'tStartRefresh')  # time at next scr refresh
            # add timestamp to datafile
            thisExp.timestampOnFlip(win, 'key_resp.started')
            # update status
            key_resp.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(key_resp.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(key_resp.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if key_resp.status == STARTED and not waitOnFlip:
            theseKeys = key_resp.getKeys(keyList=["num_1","num_2"], waitRelease=False)
            _key_resp_allKeys.extend(theseKeys)
            if len(_key_resp_allKeys):
                key_resp.keys = _key_resp_allKeys[-1].name  # just the last key pressed
                key_resp.rt = _key_resp_allKeys[-1].rt
                # was this correct?
                if (key_resp.keys == str('amount')) or (key_resp.keys == 'amount'):
                    key_resp.corr = 1
                else:
                    key_resp.corr = 0
                # a response ends the routine
                continueRoutine = False
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
            if eyetracker:
                eyetracker.setConnectionState(False)
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            routineForceEnded = True
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in resp_2Components:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # --- Ending Routine "resp_2" ---
    for thisComponent in resp_2Components:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # check responses
    if key_resp.keys in ['', [], None]:  # No response was made
        key_resp.keys = None
        # was no response the correct answer?!
        if str('amount').lower() == 'none':
           key_resp.corr = 1;  # correct non-response
        else:
           key_resp.corr = 0;  # failed to respond (incorrectly)
    # store data for second (TrialHandler)
    second.addData('key_resp.keys',key_resp.keys)
    second.addData('key_resp.corr', key_resp.corr)
    if key_resp.keys != None:  # we had a response
        second.addData('key_resp.rt', key_resp.rt)
    # the Routine "resp_2" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    thisExp.nextEntry()
    
# completed 6.0 repeats of 'second'


# --- Prepare to start Routine "bye" ---
continueRoutine = True
# update component parameters for each repeat
# keep track of which components have finished
byeComponents = [text_bye]
for thisComponent in byeComponents:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
frameN = -1

# --- Run Routine "bye" ---
routineForceEnded = not continueRoutine
while continueRoutine and routineTimer.getTime() < 10.0:
    # get current time
    t = routineTimer.getTime()
    tThisFlip = win.getFutureFlipTime(clock=routineTimer)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *text* updates
    
    # if text is starting this frame...
    if text_bye.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        text_bye.frameNStart = frameN  # exact frame index
        text_bye.tStart = t  # local t and not account for scr refresh
        text_bye.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(text_bye, 'tStartRefresh')  # time at next scr refresh
        # add timestamp to datafile
        thisExp.timestampOnFlip(win, 'text_bye.started')
        # update status
        text_bye.status = STARTED
        text_bye.setAutoDraw(True)
    
    # if text is active this frame...
    if text_bye.status == STARTED:
        # update params
        pass
    
    # if text is stopping this frame...
    if text_bye.status == STARTED:
        # is it time to stop? (based on global clock, using actual start)
        if tThisFlipGlobal > text_bye.tStartRefresh + 10-frameTolerance:
            # keep track of stop time/frame for later
            text_bye.tStop = t  # not accounting for scr refresh
            text_bye.frameNStop = frameN  # exact frame index
            # add timestamp to datafile
            thisExp.timestampOnFlip(win, 'text_bye.stopped')
            # update status
            text_bye.status = FINISHED
            text_bye.setAutoDraw(False)
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
        if eyetracker:
            eyetracker.setConnectionState(False)
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        routineForceEnded = True
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in byeComponents:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# --- Ending Routine "bye" ---
for thisComponent in byeComponents:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
# using non-slip timing so subtract the expected duration of this Routine (unless ended on request)
if routineForceEnded:
    routineTimer.reset()
else:
    routineTimer.addTime(-10.000000)






# --- End experiment ---
# Flip one final time so any remaining win.callOnFlip() 
# and win.timeOnFlip() tasks get executed before quitting
win.flip()

# these shouldn't be strictly necessary (should auto-save)
thisExp.saveAsWideText(filename+'.csv', delim='auto')
thisExp.saveAsPickle(filename)
logging.flush()
# make sure everything is closed down
if eyetracker:
    eyetracker.setConnectionState(False)
thisExp.abort()  # or data files will save again on exit
win.close()
core.quit()
