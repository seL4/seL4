verbatimtex
\documentclass[12pt,a4paper]{article}
\usepackage[T1]{fontenc}
\usepackage{mathptmx}
\begin{document}
etex;

input boxes;
beginfig(1);

defaultfont := "ptmr8r";

boxjoin(a.sw = b.nw; a.se = b.ne);
boxit.slen(btex $n + m$ etex);
boxit.sbadge("Badge");
boxit.shmrs("Registers");
shmrs.s = shmrs.n - (0bp, 40bp);
boxit.sbmrs("Buffer Data");
sbmrs.s = sbmrs.n - (0bp, 60bp);
fixsize(sbmrs);
drawboxed(slen, sbadge, shmrs, sbmrs);
label.top("Sent Message", slen.n + (0bp, 6bp));
label.rt(btex $0$ etex, slen.ne);
label.rt(btex $1$ etex, slen.se);
label.rt(btex $2$ etex, sbadge.se);
label.rt(btex $n + 2$ etex, shmrs.se);
label.rt(btex $n + m + 2$ etex, sbmrs.se);

boxjoin(a.sw = b.nw; a.se = b.ne);
boxit.rlen(btex $n$ etex);
rlen.nw = slen.ne + (96bp, 0bp);
rlen.sw = slen.se + (96bp, 0bp);
rlen.ne = rlen.nw + slen.ne - slen.nw;
boxit.rbadge("Badge");
boxit.rhmrs("Registers");
rhmrs.s = rhmrs.n - (0bp, 40bp);
fixsize(rhmrs);
drawboxed(rlen, rbadge, rhmrs);
label.top("Received Message", rlen.n + (0bp, 6bp));
label.rt(btex $0$ etex, rlen.ne);
label.rt(btex $1$ etex, rlen.se);
label.rt(btex $2$ etex, rbadge.se);
label.rt(btex $n + 2$ etex, rhmrs.se);

drawarrow(sbadge.e -- rbadge.w) dashed evenly;
drawarrow(shmrs.e -- rhmrs.w) dashed evenly;

endfig;
end.

