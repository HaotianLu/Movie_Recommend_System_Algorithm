# Project 4: Algorithm implementation and evaluation: Collaborative Filtering

### [Project Description](doc/project4_desc.md)

Term: Fall 2019

+ Team # 9
+ Projec title: Movie Recommendation System
+ Team members
	+ Yanzhu Chen (yc3511)
	+ Sagar Lal (SL3946)
	+ Haotian Lu (hl3089)
	+ Yakun Wang (yw3211)
	+ Yizhen Xu (yx2470)

+ Project summary: In this project we explore different approaches to matrix factorization on ml-small dataset. Specifically, when performing matrix factorization with user and movie biases, as well as global bias, we explore what post-processing will improve performance. When we apply KNN post processing we see few improvements as suggested by paper 2. Meanwhile, after applying KRR the results improve prior to linear regression.
	
**Contribution statement**: All team members contributed equally in all stages of this project. All team members approve our work presented in this GitHub repository including this contributions statement. 

  + Yanzhu Chen: Read Papers, KRR, KNN Post Processing, High rating accuracy threshold and calculation
  + Sagar Lal: Read Papers, User Based KNN Post Processing, KNN main.rmd, readme file
  + Haotian Lu: Read Papers, ALS+R1R2 algorithm, organize process, support group members 
  + Yakun Wang: Read Papers, ALS, KNN, KRR function and cross validation in R, generate main.rmd, High rating accuracy idea
  + Yizhen Xu: Read Papers, KRR Post Processing, High rating accuracy Threshold, Slides and presentation 

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.
