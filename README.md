# history_hsv-manager_LS-Dyna
A manager for LS-Dyna Fortran to store and retrieve data easily in the list of history variables `hsv`.

This enables you to centrally organise which history variable is stored at which location and easily reorganise your history. For instance, if you want to retrieve your damage variable in the user-material from the history, you usually write `damage_n = hsv(9)`, if the damage is stored at the 9th entry of the list of history variables `hsv`. Now imagine, you clean or extend your material model and now the damage should be stored in position 8. You would need to go through every line of code, where you retrieve or save the damage into `hsv(9)` and change this. Sounds error-prone, like a waste of time and very cumbersome. With this hsv-manager you just write  `damage_n = hsv_get_scalar('damage', hsv)` to retrieve the damage and `call hsv_set_scaler(damage_n,'damage',hsv)` to store the damage back into `hsv` and only in a single central (!) place change the ordering of your history variables.

If you now wonder about efficiency. Of course it is faster to use for instance `hsv(9)` and calling the hsv-manager. Regard the hsv-manager as a good tool for code development. Once your material model is perfect/verified/validated/tested, it is very easy to replace the hsv-manager calls by 'hsv(9)' etc. to get the last millisecond in efficiency. (Just note that a code is never perfect, always requires support/debugging/bug fixes/extensions/changes/optimisations/etc.)

## Usage and setup
Open the file 'hsv_get_sth.F' and change the names, indices and lengths of the variables you want to store in the history list 'hsv'. As shown, you can introduce multiple hsv-variants. For instance, if one of your material models only considers plasticity, you do not have to store the damage variable and related values. This simply saves memory.

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
To use the different hsv-variants, you need to initialise the hsv. This must be done before any value can be extracted or saved into hsv. For this call,
```fortran
  call hsv_init(hsv, cm_all)
```
Here, `cm_all` is a bit special (see [cm-maanager](https://github.com/jfriedlein/material-parameter_manager_LS-Dyna)), but basically you store the hsv-variant (1,2,3,...) into the first entry of hsv, so e. g. `hsv(1)=3` to use the third hsv-variant.

## Example
@todo add an example on the usage to get and set e.g. the hardening variable for elasto-plasticity
