# history_hsv-manager_LS-Dyna
A manager for LS-Dyna Fortran to store and retrieve e.g. tensorial quantities in the list of history variables hsv.

## Usage and setup
Open the file 'hsv_get_sth.F' and change the names, indices and lengths of the variables you want to store in the history list 'hsv'. You can store more than 5 quantities, however then you cannot store the pairs (hsv_init1) in only the first hsv(1) entry. If you use the latter feature, you might have to expand the init to hsv(2) ...

## Set
To set a value call the according subroutine
```fortran
  call hsv_set_X(X,'X',hsv) ! put the value of X as 'X' into the history
```

## GET
To get a value from the history call the function
```fortran
  X = hsv_get_X('X', hsv) ! get me the value of 'X' from the history
```

## INIT
You can store the start indices in hsv(1), so it becomes transparent which entry in the history list contains what. For instance,
```fortran
  hsv(1) = 0902030045 ! actually stored without leading zeros as integer
```

## CONVENTION
* first pair: start index of tangent es
* last pair:  start index of deformation gradient
* intermediate pairs: history such as alpha, eps_p, damage

So for the above example, we now know that
* 'es' starts at position 9
* 'alpha' is at index 2
* 'eps_p' starts at index 3
* 'defoGrad' can be found starting at index 45

## Example
@todo add an example on the usage to get and set e.g. the hardening variable for elasto-plasticity
