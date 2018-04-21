# -*- coding: utf-8 -*-
"""
Created on Tue Apr 10 23:19:24 2018

@author: SrivatsanPC
"""
from pandas import DataFrame
from keras.models import Sequential
from keras.layers import Dense
from keras.layers import LSTM
import random
from tqdm import tqdm

# binary encode an input pattern, return a list of binary vectors
def encode(pattern, n_unique):
	encoded = list()
	for value in pattern:
		row = [0.0 for x in range(n_unique)]
		row[value] = 1.0
		encoded.append(row)
	return encoded

# create input/output pairs of encoded vectors, returns X, y
def to_xy_pairs(encoded):
	X,y = list(),list()
	for i in range(1, len(encoded)):
		X.append(encoded[i-1])
		y.append(encoded[i])
	return X, y

# convert sequence to x/y pairs ready for use with an LSTM
def to_lstm_dataset(sequence, n_unique):
	# one hot encode
	encoded = encode(sequence, n_unique)
	# convert to in/out patterns
	X,y = to_xy_pairs(encoded)
	# convert to LSTM friendly format
	dfX, dfy = DataFrame(X), DataFrame(y)
	lstmX = dfX.values
	lstmX = lstmX.reshape(lstmX.shape[0], 1, lstmX.shape[1])
	lstmY = dfy.values
	return lstmX, lstmY

# define sequences
n_seq_train = 50
n_seq_test = 10
n_unique = 6
n_len = 3

seqsX = []
seqsY = []
seqs = []
rand_list = [random.randint(0,n_unique-1) for _ in range(n_len)]

for i in range(n_seq_train):
    seed = random.randint(0,n_unique-1)
    seqs.append([seed] + rand_list + [seed])
    lstm_data =  to_lstm_dataset(seqs[i], n_unique)    
    seqsX.append(lstm_data[0])
    seqsY.append(lstm_data[1])

test_seqsX = []
test_seqsY = []
test_seqs = []
for i in range(n_seq_test):
    seed = random.randint(0,n_unique-1)
    test_seqs.append([seed] + rand_list + [seed])
    lstm_data =  to_lstm_dataset(test_seqs[i], n_unique)    
    test_seqsX.append(lstm_data[0])
    test_seqsY.append(lstm_data[1])
    
# define LSTM configuration
n_neurons = 20
n_batch = 1
n_epoch = 250
n_features = n_unique

# create LSTM
model = Sequential()
model.add(LSTM(n_neurons, batch_input_shape=(n_batch, 1, n_features), stateful=True))
model.add(Dense(n_unique, activation='sigmoid'))
model.compile(loss='binary_crossentropy', optimizer='adam')

# train LSTM
for i in tqdm(range(n_epoch)):
   for j in range(n_seq_train):
    	model.fit(seqsX[j], seqsY[j], epochs=1, batch_size=n_batch, verbose=0, shuffle=False)
    	model.reset_states()

truths = []
# test LSTM on sequence 1
for j in range(n_seq_test):
    print('Sequence ', j)
    result = model.predict_classes(test_seqsX[j], batch_size=n_batch, verbose=0)
    model.reset_states()
    
    for i in range(len(result)):
        print('X=%.1f y=%.1f, yhat=%.1f' % (test_seqs[j][i], test_seqs[j][i+1], result[i]))
    truths.append(result[-1] == test_seqs[j][0])

print("Overall learning accuracy : ", sum(truths)/len(truths) )
    
   


