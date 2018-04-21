# -*- coding: utf-8 -*-
"""
Created on Wed Apr 11 00:08:15 2018

@author: SrivatsanPC
"""
from numpy import array
from keras.preprocessing.text import Tokenizer
from keras.utils import to_categorical
from keras.preprocessing.sequence import pad_sequences
from keras.models import Sequential
from keras.layers import Dense
from keras.layers import LSTM
from keras.layers import Embedding

# generate a sequence from a language model
def generate_seq(model, tokenizer, max_length, seed_text, n_words):
	in_text = seed_text
	# generate a fixed number of words
	for _ in range(n_words):
		# encode the text as integer
		encoded = tokenizer.texts_to_sequences([in_text])[0]
		# pre-pad sequences to a fixed length
		encoded = pad_sequences([encoded], maxlen=max_length, padding='pre')
		# predict probabilities for each word
		yhat = model.predict_classes(encoded, verbose=0)
		# map predicted word index to word
		out_word = ''
		for word, index in tokenizer.word_index.items():
			if index == yhat:
				out_word = word
				break
		# append to input
		in_text += ' ' + out_word
	return in_text

## source text
#data = """ Jack and Jill went up the hill\n
#		To fetch a pail of water\n
#		Jack fell down and broke his crown\n
#		And Jill came tumbling after\n """

data = """To be, or not to be, that is the question\n
Whether it is nobler in the mind to suffer\n
The slings and arrows of outrageous fortune\n
Or to take arms against a sea of troubles\n
And by opposing end them. To die—to sleep \n
No more; and by a sleep to say we end \n
The heart-ache and the thousand natural shocks \n
That flesh is heir to: 'tis a consummation \n
Devoutly to be wish'd. To die, to sleep \n
To sleep, perchance to dream—ay, there's the rub \n
For in that sleep of death what dreams may come \n 
When we have shuffled off this mortal coil\n
Must give us pause—there's the respect\n
That makes calamity of so long life\n"""

# prepare the tokenizer on the source text
tokenizer = Tokenizer()
tokenizer.fit_on_texts([data])
# determine the vocabulary size
vocab_size = len(tokenizer.word_index) + 1
print('Vocabulary Size: %d' % vocab_size)
# create line-based sequences
sequences = list()
for line in data.split('\n'):
	encoded = tokenizer.texts_to_sequences([line])[0]
	for i in range(1, len(encoded)):
		sequence = encoded[:i+1]
		sequences.append(sequence)
print('Total Sequences: %d' % len(sequences))
# pad input sequences
max_length = max([len(seq) for seq in sequences])
sequences = pad_sequences(sequences, maxlen=max_length, padding='pre')
print('Max Sequence Length: %d' % max_length)
# split into input and output elements
sequences = array(sequences)
X, y = sequences[:,:-1],sequences[:,-1]
y = to_categorical(y, num_classes=vocab_size)
# define model
model = Sequential()
model.add(Embedding(vocab_size, 10, input_length=max_length-1))
model.add(LSTM(50))
model.add(Dense(vocab_size, activation='softmax'))
print(model.summary())
# compile network
model.compile(loss='categorical_crossentropy', optimizer='adam', metrics=['accuracy'])
# fit network
model.fit(X, y, epochs=500, verbose=2)
# evaluate model
print(generate_seq(model, tokenizer, max_length-1, 'The slings and', 4))
print(generate_seq(model, tokenizer, max_length-1, 'Or to', 4))
print(generate_seq(model, tokenizer, max_length-1, 'Must give', 4))

