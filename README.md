ocpcheck
========

Ocpcheck goal is to generate feasible or unfeasible problems of a certain type in order to check solvers.

The problem is :

          max C x

s.t. lbg <= A x <= ubg

s.t.    lbx <= x <= ubx

s.t.    lby <= y <= uby

---
The files RandGLPK, Rand and Generate focuses on problems with only one constraint and a two-dimension space. Generate.hs uses the hmatrix-glpk solver to test RandGLPK.

The file LPGenerate focuses on LP problems of any dimensions (space and constraints). The file QPGenerate does the same but for an objective function of the following kind : xQx + Cx

The file CreateText generates a *.hs file which can then be used to test Greg Horn's NLP sover from dynobud.
the file TestHigherDim tests LPGenerate with the hmatrix-glpk solver.

---

This is how I proceeded for the beginning:
The module RandGLPK defines new types : 
  Param : the parameters a, b, c, d
  Bounds : the bounds for the constraints lbx,ubx,lby,uby,lbg,ubg
  
The function generateParameters generates the parameters and bounds for a feasible (or not) problem within two boundaries.

The function createBoundsF generates first randomly all the parameters and the bounds lbx,ubx,lby and uby. 
This enables all the possibilities for those parameters.
For the last two, since we want the constraint lbg <= c x + d y <= ubg not to be empty, 
it means that we want either lbg or ubg to be in the possible range of (c x + d y)
That's why we only need to generate one of these bounds inside that range and the other one doesn't matter 
(that's why we generate it with the other parameters in the beginning)

The function createBoundsU uses the contrary of what we just said.
We want the constraint lbg <= c x + d y <= ubg to be empty (or bounds unfeasible (lower bound > upper bound) ),
it means that we want both lbg and ubg to be out of the possible range of (c x + d y)
That's why we generate all the parameters and bounds, check if the bounds are feasible or not, and if they are feasible,
we check if  both lbg and ubg to be out of the possible range of (c x + d y).
If not we create them (using genGBoundsU2) so they are either both greater than the upper bound of the possible (c x + d y) or both smaller
(using a randomly generated boolean to decide whether they will be greater or smaller) 
