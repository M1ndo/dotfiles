#!/usr/bin/python3
# Shuffler for ascii art
# made by ybenel
import random
import sys
import os
path = "/opt/color-scripts/color-scripts"
listdir = os.listdir(path) 
random.shuffle(listdir)
for i in listdir:
    name = i 
    
os.system('%s/%s'%(path,i))
os.system('stty echo')
