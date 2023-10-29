# StudentAllocate
R script for fair allocation of students to projects

Supervisors propose one topic each. 
Students choose their favourite 6 topic. 

This script takes a list of students and their project preferences, and initially allocates students randomly to a topic. 

The script then calculates a score for the entire cohort, depending on how far on the preference list each student's topic is. 

Then, some random substitutions are made, and a new score is calculated. If the score is lower than the first one, then the new arrangement is kept, and another substitution is made. If the score is higher or equal, the initial arrangement is kept, and another substitution is made. 

Number of cycles of substitution can be set depending on cohort size. 

