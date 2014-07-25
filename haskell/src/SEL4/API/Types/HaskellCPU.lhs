%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module contains an instance of the machine-specific kernel API for the Haskell-based CPU simulator. This simulator is for the machine-independent API, and defines no additional object types; so this module re-exports the definitions of the universal object types.

> module SEL4.API.Types.HaskellCPU (module SEL4.API.Types.Universal) where

> import SEL4.API.Types.Universal


