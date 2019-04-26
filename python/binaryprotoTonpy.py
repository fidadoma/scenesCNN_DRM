#In the name of God
#Use this script to convert a binaryproto mean file to an equivalent python npy mean file.
#simply use the script like this : 
#python binaryprotoTonpy.py mean.binaryproto mean.npy"

import numpy as np
import sys
import os

caffe_root = 'd:/Documents/caffe'
sys.path.insert(0, os.path.join(caffe_root,'python'))

import caffe

if len(sys.argv) != 3:
    print "Usage: python binaryprotoTonpy.py mean.binaryproto mean.npy"
    sys.exit()

blob = caffe.proto.caffe_pb2.BlobProto()
data = open( sys.argv[1] , 'rb' ).read()
blob.ParseFromString(data)
data = np.array(blob.data)
arr = np.array( caffe.io.blobproto_to_array(blob) )
out = arr[0]
np.save( sys.argv[2] , out )