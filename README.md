# PSS_model: A neurobiological ACT-R model of the PSS Task

This repository contains the code and the simulations for an ACT-R model that reproduces the patterns of data obtained from Parkinson patients and neurotypicals from the Probabilistic Stimulus Selection (PSS) task.

## Reasons for a model 

ACT-R, like other production system-based architecture such as EPIC and Soar, implicitly assume that productioon (or operator) cycles represent an abstraction of the basal ganglia. 
This identification, however, overlooks the fact that the BG have two opposing pathways.

## Model architecture

To implement the two pathways, ACT-R is modified to include opposing "Do" and "Don't" productions, which represent the excitary effects of the direct pathways and the inhibitory effects of the indirect pathway, respectively.
The two series of productions have two associated parameters, d1 and d2, which represent the density of dopaminergic D1 and D2 receptors and modulate the learning rate alpha.

## The PSS Task

The model was tested with the PSS task, a two-alternative forced-choice task introduced by Michael Frank in 2004. The task produces two behavioral measures, Choose and Avoid, which reflect the contributions of the direct and indirect pathway. 

In a number of experiments, Frank has shown that 

(1) PD patients off dopamine show increased Avoid scores and reduced Choose scores; but 
(2) This pattern is reversed when patients are tested on dopamine; and 
(3) Neurotypicals whose genes express more D2 receptors also show increased Avoid and reduce Choose scores; and
(4) Neurotypicals whose genes express more D1 receptors show increased Choose and reduce Avoid scores.

## The models

The repo contains two models, a default ACT-R model of the PSS task and a modified model with the competitive productions.

The results of the simulations show that only the competitive model can capture the patterns of results described above. 

