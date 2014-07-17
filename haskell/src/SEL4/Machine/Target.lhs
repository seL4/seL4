%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module is exported to the simulator interface to allow it to access target-dependent definitions.

> {-# LANGUAGE CPP #-}

> module SEL4.Machine.Target (
>         module SEL4.Machine.Hardware.TARGET,
>         module SEL4.Machine.RegisterSet.TARGET
>     ) where
> 
> import SEL4.Machine.Hardware.TARGET
> import SEL4.Machine.RegisterSet.TARGET
