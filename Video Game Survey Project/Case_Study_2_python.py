#Question 1
import numpy as np
lst=[]
for i in range(197):
 lst.append(0)
for j in range(117):
 lst.append(1)
standard=np.std(lst)
average=np.mean(lst)
CI=(average-1.96*(standard/ np.sqrt(91)),
    average+1.96*(standard/ np.sqrt(91)))

#Bootstrapping
random.seed(10)
n_iteration=10000
new_lst=[]
for i in range(n_iteration):
    sample= resample(lst,n_samples=91)
    average=np.mean(sample)
    new_lst.append(average)
new_lst.sort()
plt.hist(new_lst, bins='auto')
plt.title("Bootstrapping the fraction of students who played'")
plt.savefig('who play.png')
lower_bound=np.percentile(new_lst,2.5)
upper_bound=np.percentile(new_lst,97.5)
print('lower bound is :',lower_bound)
print('upper bound is :',upper_bound)
print('95% Confidence Interval is :',(lower_bound,upper_bound))

#Question 3
import numpy as np
from sklearn.utils import resample
import matplotlib.pyplot as plt
import random

#create the same list for column "time"
lst=[]
for i in range(int(57*314/91)):
     lst.append(0)
for j in range(int(14*314/91)):
    lst.append(2)
for l in range(int(5*314/91)):
    lst.append(0.5)
for k in range(int(3*314/91)):
    lst.append(3)
for m in range(int(5*314/91)):
    lst.append(1)
lst.append(14)
lst.append(14)
lst.append(14)
lst.append(14)
lst.append(14)
lst.append(14)
lst.append(0.1)
lst.append(0.1)
lst.append(0.1)
lst.append(1.5)
lst.append(1.5)
lst.append(1.5)
lst.append(30)
lst.append(30)
lst.append(30)
lst.append(4)
lst.append(4)
lst.append(4)
lst.append(5)
lst.append(5)
lst.append(5)
#bootstrap
random.seed(10)
n_iteration=10000
new_lst=[]
for i in range(n_iteration):
    sample= np.random.choice(lst,size=91,replace=False)
    average=np.mean(sample)
    new_lst.append(average)
new_lst.sort()
plt.hist(new_lst, bins='auto')
plt.title("Bootstrap of 'time'(hours)")
plt.savefig('Bootstrap.png')
#95% Confidence Interval
lower_bound=np.percentile(new_lst,2.5)
upper_bound=np.percentile(new_lst,97.5)
print('lower bound is :',lower_bound)
print('upper bound is :',upper_bound)
print('95% Confidence Interval is :',(lower_bound,upper_bound))

#Question 6
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.ticker import PercentFormatter

#create a 'grade' list, change the number to letter grade
grade=[]
for i in range(31):
    grade.append('A')
for j in range(52):
    grade.append('B')
for k in range(8):
    grade.append('C')

plt.hist(grade, weights=np.ones(len(grade)) / len(grade))
plt.title("Expected Grade of the students")
plt.gca().yaxis.set_major_formatter(PercentFormatter(1))
plt.show()

new=[]
number1=random.sample(('A','B','C','D'),1)
number2=random.sample(('A','B','C','D'),1)
number3=random.sample(('A','B','C','D'),1)
number4=random.sample(('A','B','C','D'),1)
new.append(number1)
new.append(number2)
new.append(number3)
new.append(number4)
for i in range(len(new)):
    grade.append(new[i][0])
plt.hist(grade, weights=np.ones(len(grade)) / len(grade))
plt.title("Expected Grade of the students")
plt.gca().yaxis.set_major_formatter(PercentFormatter(1))
plt.show()
