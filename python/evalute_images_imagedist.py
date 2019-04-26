import numpy as np
import os
import sys
import datetime as dtime
import scipy as scipy
from more_itertools import flatten

# The caffe module needs to be on the Python path;
### CHANGE PATH TO BE ACCORDING TO YOUR INSTALLATION
caffe_root = 'd:/Documents/caffe/'

sys.path.insert(0, os.path.join(caffe_root,'python'))
import caffe

caffe.set_device(0) 
caffe.set_mode_gpu() 

fdir = 'd:/Documents/figrim2_clean' # images directory, only clean version
model_dir = "models/places365/" # model path

out_dir = 'd:/Documents/git/scenesCNN_DRM/data/figrim_vectors'
if not os.path.exists(out_dir):
    os.makedirs(out_dir)
	
bs = 75  # batch size, depends on your computer's power, if python crashes decrease
l2 = True    # compute L2 distance matrix (mildly computationaly demanding)
corr = False  # compute correlation matrix (very computationaly demanding)
lab = True   # save top5 predictions with labels (not computationaly demanding)

loud = False # verbose

model_def = caffe_root + model_dir + 'deploy_alexnet_places365.prototxt'
model_weights = caffe_root + model_dir + 'alexnet_places365.caffemodel'
labels_file = caffe_root + model_dir + 'categories_places365.txt'
labels = np.loadtxt(labels_file, str, delimiter='\t')

time_start = dtime.datetime.now().replace(microsecond=0)
print "starting at " + str(time_start.hour) + ":" + str(time_start.minute)
fnames = os.listdir(fdir)



print "number of images: " + str(len(fnames))


### SET UP NETS
# rows net
net1 =  caffe.Net(model_def,model_weights,caffe.TEST)
    
### SOME SETTINGS MAGIC
# load the mean ImageNet image (as distributed with Caffe) for subtraction
mu = np.load(caffe_root + 'python/caffe/imagenet/ilsvrc_2012_mean.npy')
mu = mu.mean(1).mean(1)  # average over pixels to obtain the mean (BGR) pixel values

# create transformer for the input called 'data'
transformer1 = caffe.io.Transformer({'data': net1.blobs['data'].data.shape})


transformer1.set_transpose('data', (2,0,1))  # move image channels to outermost dimension
transformer1.set_mean('data', mu)            # subtract the dataset-mean value in each channel
transformer1.set_raw_scale('data', 255)      # rescale from [0, 1] to [0, 255]
transformer1.set_channel_swap('data', (2,1,0))  # swap channels from RGB to BGR

net1.blobs['data'].reshape(1, 3, 227, 227)

out_l = list()
loud = True
### START LOOPIN'
cur_loop = 1

if loud: print "\ncreating new net"
 
 
if loud: print "loading images: "

for i, f in enumerate(fnames):
            
    image = caffe.io.load_image(fdir + "/" + f)
    net1.blobs['data'].data[:, :,:,:] = transformer1.preprocess('data', image)
    
    if loud: print i,              
    
        
    out1 = net1.forward()
    if loud: print "- "
	
    if loud: print max(out1['prob'][0])	
    
    x = net1.blobs['fc7'].data[0]
    #out_l.append(x)
    np.savetxt(os.path.join(out_dir,"figrim_fc7_"+ f + ".txt"), x, delimiter=",")
# print out_l



with open(os.path.join(out_dir,"figrim_imgnames" + ".txt"), 'w') as filehandle:  
    for listitem in fnames:
        filehandle.write('%s\n' % listitem)


