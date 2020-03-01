% SPDX-License-Identifier: LPPL-1.3c

%%
%% This is file `export.bst',
%% generated with the docstrip utility.
%%
%% The original source files were:
%%
%% bibexport.dtx  (with options: `export')
%% 
%% (c) 2005/02/27 Nicolas Markey <markey at lsv dot ens-cachan dot fr>
%% All rights reserved.
%% 
%% This work may be distributed and/or modified under the conditions of
%% the LaTeX Project Public License, either version 1.3 of this license
%% or (at your option) any later version. The latest version of the
%% license is in
%% 
%%     http://www.latex-project.org/lppl.txt
%% 
%% and version 1.3 or later is part of all distributions of LaTeX
%% version 2003/12/01 or later.
%% 
%% \CharacterTable
%%  {Upper-case    \A\B\C\D\E\F\G\H\I\J\K\L\M\N\O\P\Q\R\S\T\U\V\W\X\Y\Z
%%   Lower-case    \a\b\c\d\e\f\g\h\i\j\k\l\m\n\o\p\q\r\s\t\u\v\w\x\y\z
%%   Digits        \0\1\2\3\4\5\6\7\8\9
%%   Exclamation   \!     Double quote  \"     Hash (number) \#
%%   Dollar        \$     Percent       \%     Ampersand     \&
%%   Acute accent  \'     Left paren    \(     Right paren   \)
%%   Asterisk      \*     Plus          \+     Comma         \,
%%   Minus         \-     Point         \.     Solidus       \/
%%   Colon         \:     Semicolon     \;     Less than     \<
%%   Equals        \=     Greater than  \>     Question mark \?
%%   Commercial at \@     Left bracket  \[     Backslash     \\
%%   Right bracket \]     Circumflex    \^     Underscore    \_
%%   Grave accent  \`     Left brace    \{     Vertical bar  \|
%%   Right brace   \}     Tilde         \~}
%%
FUNCTION{left.width}{#18}
FUNCTION{right.width}{#55}
FUNCTION{url.right.width}{#61}
FUNCTION{left.short.width}{#10} %% for @preamble
FUNCTION{right.long.width}{#63}
FUNCTION{left.delim}{"{"}
FUNCTION{right.delim}{"}"}
ENTRY{
    address
    author
    booktitle
    chapter
    edition
    editor
    howpublished
    institution
    journal
    key
    month
    note
    number
    organization
    pages
    publisher
    school
    series
    title
    type
    volume
    year
    abstract
    doi
    eid
    isbn
    issn
    language
    url
}{}{}
FUNCTION{not}
{
    {#0}
    {#1}
  if$
}
FUNCTION{and}
{
    'skip$
    {pop$ #0}
  if$
}
FUNCTION{or}
{
    {pop$ #1}
    'skip$
  if$
}
INTEGERS{left.length right.length}
STRINGS{ s t }
FUNCTION{space.complete}
{
  'left.length :=
  duplicate$ text.length$ left.length swap$ -
  {duplicate$ #0 >}
    {
      swap$ " " * swap$ #1 -
    }
  while$
  pop$
}
FUNCTION{split.string}
{
  'right.length :=
  duplicate$ right.length #1 + #1 substring$ "" =
    {""}
    {
      's :=
      right.length
      {duplicate$ duplicate$ s swap$ #1 substring$ " " = not and}
        {#1 -}
      while$
      duplicate$ #2 <
        {
          pop$ "    " s * ""
        }
        {
          duplicate$ s swap$ #1 swap$ substring$
          swap$
          s swap$ global.max$ substring$
        }
      if$
    }
  if$
}
FUNCTION{split.url}
{
  'right.length :=
  duplicate$ right.length #1 + #1 substring$ "" =
    {""}
    {
      's :=
      right.length
      {duplicate$ duplicate$ s swap$ #1 substring$ "/" = not and}
        {#1 -}
      while$
      duplicate$ #2 <
        {
          pop$ "    " s * ""
        }
        {
          duplicate$ s swap$ #1 swap$ substring$
          swap$ #1 +
          s swap$ global.max$ substring$
        }
      if$
    }
  if$
}
FUNCTION{split.name}
{
  'right.length :=
  duplicate$ right.length #1 + #1 substring$ "" =
    {""}
    {
      's :=
      right.length
      {duplicate$ duplicate$ s swap$ #5 substring$ " and " = not and}
        {#1 -}
      while$
      duplicate$ #2 <
        {
          pop$ "  " s * ""
        }
        {
          #4 + duplicate$ s swap$ #1 swap$ substring$
          swap$
          s swap$ global.max$ substring$
        }
      if$
    }
  if$
}
FUNCTION{field.export}
{
  duplicate$ missing$
    'skip$
    {
      left.delim swap$ * right.delim *
      swap$
      "  " swap$ * " = " * left.width space.complete
      swap$ "," *
      {duplicate$ "" = not}
        {
          right.width split.string 't :=
          *
          write$ newline$
          "" left.width space.complete t
        }
      while$
    }
  if$
  pop$ pop$
}
FUNCTION{abbrv.export}
{
  duplicate$ missing$
    'skip$
    {
      swap$
      "  " swap$ * " = " * left.width space.complete
      swap$ "," *
      {duplicate$ "" = not}
        {
          right.width split.string 't :=
          *
          write$ newline$
          "" left.width space.complete t
        }
      while$
    }
  if$
  pop$ pop$
}
FUNCTION{name.export}
{
  duplicate$ missing$
    'skip$
    {
      left.delim swap$ * right.delim *
      swap$
      "  " swap$ * " = " * left.width space.complete
      swap$ "," *
      {duplicate$ "" = not}
        {
          right.width split.name 't :=
          *
          write$ newline$
          "" left.width space.complete t
        }
      while$
    }
  if$
  pop$ pop$
}
FUNCTION{url.export}
{
  duplicate$ missing$
    'skip$
    {
      left.delim swap$ * right.delim *
      swap$
      "  " swap$ * " = " * left.width space.complete
      swap$ "," *
      {duplicate$ "" = not}
        {
          url.right.width split.url 't :=
          *
          write$ newline$
          "" left.width space.complete t
        }
      while$
    }
  if$
  pop$ pop$
}
MACRO{jan}{"export-jan"}
MACRO{feb}{"export-feb"}
MACRO{mar}{"export-mar"}
MACRO{apr}{"export-apr"}
MACRO{may}{"export-may"}
MACRO{jun}{"export-jun"}
MACRO{jul}{"export-jul"}
MACRO{aug}{"export-aug"}
MACRO{sep}{"export-sep"}
MACRO{oct}{"export-oct"}
MACRO{nov}{"export-nov"}
MACRO{dec}{"export-dec"}
MACRO{acmcs}{"export-acmcs"}
MACRO{acta}{"export-acta"}
MACRO{cacm}{"export-cacm"}
MACRO{ibmjrd}{"export-ibmjrd"}
MACRO{ibmsj}{"export-ibmsj"}
MACRO{ieeese}{"export-ieeese"}
MACRO{ieeetc}{"export-ieeetc"}
MACRO{ieeetcad}{"export-ieeetcad"}
MACRO{ipl}{"export-ipl"}
MACRO{jacm}{"export-jacm"}
MACRO{jcss}{"export-jcss"}
MACRO{scp}{"export-scp"}
MACRO{sicomp}{"export-sicomp"}
MACRO{tocs}{"export-tocs"}
MACRO{tods}{"export-tods"}
MACRO{tog}{"export-tog"}
MACRO{toms}{"export-toms"}
MACRO{toois}{"export-poois"}
MACRO{toplas}{"export-toplas"}
MACRO{tcs}{"export-tcs"}
INTEGERS{ intxt }
FUNCTION{remove.exports.from.months}
{
  #0 'intxt :=
  duplicate$ missing$
    'skip$
    {'t :=
    ""
    {t #1 #1 substring$ "" = not}
      {
      t #1 #7 substring$ "export-" =
        {intxt
           {right.delim * #0 'intxt :=}
           'skip$
         if$
         duplicate$ "" =
           'skip$
           {" # " *}
         if$
         t #8 #3 substring$ *
         t #11 global.max$ substring$ 't :=}
        {intxt
           'skip$
           {duplicate$ "" =
              {}
              {" # " *}
            if$
            left.delim * #1 'intxt :=}
         if$
         t #1 #1 substring$ *
         t #2 global.max$ substring$ 't :=}
      if$
      }
    while$
    intxt
      {right.delim *}
      'skip$
    if$
    }
  if$
}
FUNCTION{remove.export.from.journals}
{
  duplicate$ missing$
    'skip$
    {
      duplicate$ #1 #7 substring$ "export-" =
        {#8 global.max$ substring$}
        {left.delim swap$
         right.delim * *}
      if$
    }
  if$
}
FUNCTION{entry.export.standard}
{
  "address" address field.export
  "author" author name.export
  "booktitle" booktitle field.export
  "chapter" chapter field.export
  "crossref" crossref field.export
  "edition" edition field.export
  "editor" editor name.export
  "howpublished" howpublished field.export
  "institution" institution field.export
  "journal" journal remove.export.from.journals abbrv.export
  "key" key field.export
  "month" month remove.exports.from.months abbrv.export
  "note" note field.export
  "number" number field.export
  "organization" organization field.export
  "pages" pages field.export
  "publisher" publisher field.export
  "school" school field.export
  "series" series field.export
  "type" type field.export
  "title" title field.export
  "volume" volume field.export
  "year" year field.export
}
FUNCTION{entry.export.extra}
{
  "abstract" abstract field.export
  "doi" doi field.export
  "eid" eid field.export
  "isbn" isbn field.export
  "issn" issn field.export
  "language" language field.export
  "url" url url.export
}
FUNCTION{entry.export}
{
  entry.export.standard
  entry.export.extra
}
FUNCTION{export}
{
  "@" type$ * "{" * cite$ * "," * write$ newline$
  entry.export
  "}" write$ newline$ newline$
}
FUNCTION{preamble}
{
preamble$ duplicate$ "" =
  'pop$
  {
    ",-------------------." write$ newline$
    "|     PREAMBLE      |" write$ newline$
    "`-------------------'" write$ newline$ newline$
    "@preamble{ "  swap$
    quote$ swap$ * quote$ *
    {duplicate$ "" = not}
      {
        right.long.width split.string 't :=
        *
        write$ newline$
        "" left.short.width space.complete t
      }
    while$
    "}" write$ newline$ newline$
    pop$ pop$
  }
if$
}
FUNCTION{header}
{
newline$
}
FUNCTION{entries.header}
{
    ",-------------------." write$ newline$
    "|  BIBTEX ENTRIES   |" write$ newline$
    "`-------------------'" write$ newline$ newline$
}
FUNCTION{article}{export}
FUNCTION{book}{export}
FUNCTION{booklet}{export}
FUNCTION{conference}{export}
FUNCTION{habthesis}{export}
FUNCTION{inbook}{export}
FUNCTION{incollection}{export}
FUNCTION{inproceedings}{export}
FUNCTION{journals}{export}
FUNCTION{manual}{export}
FUNCTION{mastersthesis}{export}
FUNCTION{misc}{export}
FUNCTION{phdthesis}{export}
FUNCTION{proceedings}{export}
FUNCTION{techreport}{export}
FUNCTION{unpublished}{export}
READ
EXECUTE{header}
EXECUTE{preamble}
EXECUTE{entries.header}
ITERATE{export}
%% 
%%
%% End of file `export.bst'.
