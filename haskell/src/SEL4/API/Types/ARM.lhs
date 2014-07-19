%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module contains an instance of the machine-specific kernel API for the ARM architecture.

> module SEL4.API.Types.ARM where

> import SEL4.API.Types.Universal(APIObjectType, apiGetObjectSize)
> import SEL4.Machine.Hardware.ARM

There are three ARM-specific object types: virtual pages, page tables, and page directories.

> data ObjectType
>     = APIObjectType APIObjectType
>     | SmallPageObject
>     | LargePageObject
>     | SectionObject
>     | SuperSectionObject
>     | PageTableObject
>     | PageDirectoryObject
>     deriving (Show, Eq)

> instance Bounded ObjectType where
>     minBound = APIObjectType minBound
>     maxBound = PageDirectoryObject

> instance Enum ObjectType where
>     fromEnum e = case e of
>         APIObjectType a -> fromEnum a
>         SmallPageObject -> apiMax + 1
>         LargePageObject -> apiMax + 2
>         SectionObject -> apiMax + 3
>         SuperSectionObject -> apiMax + 4
>         PageTableObject -> apiMax + 5
>         PageDirectoryObject -> apiMax + 6
>         where apiMax = fromEnum (maxBound :: APIObjectType)
>     toEnum n
>         | n <= apiMax = APIObjectType $ toEnum n
>         | n == apiMax + 1 = SmallPageObject
>         | n == apiMax + 2 = LargePageObject
>         | n == apiMax + 3 = SectionObject
>         | n == apiMax + 4 = SuperSectionObject
>         | n == apiMax + 5 = PageTableObject
>         | n == apiMax + 6 = PageDirectoryObject
>         | otherwise = error "toEnum out of range for ARM.ObjectType"
>         where apiMax = fromEnum (maxBound :: APIObjectType)

> fromAPIType = APIObjectType

> toAPIType (APIObjectType a) = Just a
> toAPIType _ = Nothing

> pageType = SmallPageObject

> getObjectSize :: ObjectType -> Int -> Int
> getObjectSize SmallPageObject _ = pageBitsForSize ARMSmallPage
> getObjectSize LargePageObject _ = pageBitsForSize ARMLargePage
> getObjectSize SectionObject _ = pageBitsForSize ARMSection
> getObjectSize SuperSectionObject _ = pageBitsForSize ARMSuperSection
> getObjectSize PageTableObject _ = ptBits
> getObjectSize PageDirectoryObject _ = pdBits
> getObjectSize (APIObjectType apiObjectType) size = apiGetObjectSize apiObjectType size


