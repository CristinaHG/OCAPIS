from svmutil import *
y, x = svm_read_problem('../heart_scale')
W = [1] * len(y)
W[0] = 10
prob = svm_problem(W, y, x)
param = svm_parameter('-s 3 -c 5 -h 0')
m = svm_train([], y, x, '-c 5')
m = svm_train(W, y, x)
m = svm_train(prob, '-t 2 -c 5')
m = svm_train(prob, param)
CV_ACC = svm_train(W, y, x, '-v 3')

