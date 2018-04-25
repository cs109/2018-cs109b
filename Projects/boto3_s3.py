#Following code reads files from local machine recursively within directory.
#Uploads to S3 bucket - creates 2 folders within S3 bucket - uploads 5 files in each folder.

import boto3
import glob
import os

s3 = boto3.client('s3')

bucket1 = 'adiencebenchmark-002'

#Get list of files -local
S3Adience_DIR = '/Users/Rashmi/Downloads/adiencebenchmark-002'

files = []
for filename in glob.iglob(S3Adience_DIR + '/**/*.jpg', recursive=True):
    fname = os.path.basename(filename)
    files.append(filename)
print("Total: " , len(files), " files to process.")

#Uploading 10 files in 2 different folders on S3 (also creating folders on S3)
for fname in files[0:5]+files[155:160]:
    subdir1 = os.path.split(os.path.split(fname)[0])[1]
    
    justfname = os.path.basename(fname)
    #https://stackoverflow.com/questions/34191729/create-directories-in-amazon-s3-using-python-boto3
    
    KeyFileName = "{subdir1}/{fname}".format(subdir1=subdir1,fname=justfname)

    with open(fname, 'rb') as f :
        object_data = f.read()
        s3.put_object(Body=object_data, Bucket=bucket1, Key=KeyFileName)


