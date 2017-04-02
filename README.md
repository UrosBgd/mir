# mir

MIR is a music classification program that uses machine learning and feature extraction techniques for a genre classification.

## Dataset

Dataset used for training and testing the alghorithm can be downloaded from MARSYAS framework's website http://marsyasweb.appspot.com/download/data_sets/. Each song is a 30 sec long cut of a record. All files are in .au, 16bit, 22050Hz audio format.

## Project structure

Project is logically divided into packages, each containing specific functionality.

### db

db package contains database connection and CRUD operations for storing and retreiving song information and corresponding features.

### dsp

dsp or Digital signal processing contains FFT implementation. FFT (Fast Fourier transform) is algorithm for transforming signal from time domain into frequency domain. FFT is applied to a signal by dividing it into windows of the same size (4096 in our case) and then applying FFT on each of them.

FFT output is array of complex numbers. Almost all feature extraction algorithms we implemented have real part of FFT data as their input.

### feature

feature package contains feature extraction algorithms implementations. Those are: Constant Q, Moments, Rolloff, Spectral flux, Spectrum, Zero crossings and MFCC. Each of these features tries to describe as close as possible rhythm, melody, pitch or timbre of a song. 

As our input for Machine learning algorithm we have 22 values: Constant Q mean & std; Log Constant Q mean & std; scale, mean and centroid moments (mean & std for each); Rolloff mean & std; Spectral flux mean & std; Magnitude and Power Spectrum (mean & std for each); Zero crossings mean & std; MFCC mean & std.

### io

io containts code for importing audio signal, decoding it, reading bytes and transforming them to short array.

### mir

mir package is the main package gluing together all other parts of application.

### ml

ml or Machine Learning package contains implementation of KNN algorithm and required processes (normalization, reading from .csv etc.). Our implementation includes KNN with Euclidean distances with k = 5 nearest neighbours. Our dataset is divided into 80-20% ratio where 80% is training data and 20% testing data. Labels are: blues, classical, country, disco, hiphop, jazz, metal, pop, reggae, rock.

### util

Contains helper methods for writing data to .csv file, formatting and transforming numbers and arrays, formatting song data and statistical methods.

Copyright © 2017.
