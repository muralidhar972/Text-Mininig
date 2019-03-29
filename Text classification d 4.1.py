# -*- coding: utf-8 -*-
"""
Created on Fri Mar  8 16:04:50 2019

@author: apm61831
"""

# -*- coding: utf-8 -*-
"""
Created on Fri Mar  8 14:50:14 2019

@author: apm61831
"""


from stop_words import get_stop_words
from sklearn.feature_extraction.text import TfidfVectorizer, CountVectorizer
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import LabelEncoder
from keras.models import Sequential
from keras import layers

import pandas as pd


stop_word_p = get_stop_words('portuguese')

#Reading data
train_td2=pd.read_excel('//bredsntp008.corpnet2.com//apm61831$//re work for brazil//Text classification d//new_train_data_farma_d.xlsx')

# create a count vectorizer object 
count_vect = CountVectorizer(analyzer='word',stop_words=stop_word_p , token_pattern=r'\w{1,}')

count_vect.fit(train_td2['text'])


#spliltting the data into train and test
train_x, valid_x, y_train, y_test = train_test_split(train_td2['text'], train_td2['label'])

# transform the training and validation data using count vectorizer object
X_train=  count_vect.transform(train_x)

X_test =  count_vect.transform(valid_x)



input_dim = X_train.shape[1]  # Number of features

#creating model
model = Sequential()
model.add(layers.Dense(128, input_dim=input_dim, activation='relu'))
#model.add(layers.Dropout(0.2))
#model.add(layers.Dense(64, input_dim=input_dim, activation='relu'))
#model.add(layers.Dropout(0.2))
#model.add(layers.Dense(16, input_dim=input_dim, activation='relu'))
#model.add(layers.Dropout(0.2))
model.add(layers.Dense(1, activation='sigmoid'))


#checking with model information
model.compile(loss='binary_crossentropy', 
              optimizer='adam', 
              metrics=['accuracy'])
model.summary()


#fitting the model 
history = model.fit(X_train, y_train,
                    epochs=100,
                    verbose=False
                    ,batch_size=10)

prdt=model.predict(X_test)

prdt_cls=model.predict_classes(X_test)

prdt_prb=model.predict_proba(X_test)

pred_bd_df = pd.DataFrame(prdt_prb, columns = ['prdbt'])


pred_df = pd.DataFrame(prdt_cls, columns = ['prd'])


loss_tr, accuracy_tr= model.evaluate(X_train, y_train, verbose=False)


loss_ts, accuracy_ts = model.evaluate(X_test, y_test, verbose=False)



X_ped=model.predict(X_test)
