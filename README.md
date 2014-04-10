ocpcheck
========
The code Generate.hs is a code that generates a certain number of times problems with 2 variables. 
The number is given by the user after request (must be a number, haven't considered exceptions yet)

The problem is :
          max a x + b y
s.t. lbg <= c x + d y <= ubg
s.t.    lbx <= x <= ubx
s.t.    lby <= y <= uby

Then the machine automatically generates a random boolean and then uses the functions in RandGLPK.hs to generate the parameters and boundaries of the problem.
Then we create the problem and the constraints of a hmatrix-glpk problem-solver with those parameters and bounds.

The machine then shows if the parameters were supposed to create a fesaible problem (True) or not (False)
alongside with the result of the solver (NoFeasible, UnFeasible, Optimal {..} )
I.e :
False      ->       UnFeasible

---
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
