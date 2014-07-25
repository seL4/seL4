input boxes;
beginfig(1);

defaultfont := "ptmr8r";

boxjoin(a.sw = b.nw; a.se = b.ne);
boxit.ut("Untyped Capability");
boxit.ub("Retype Right");
drawboxed(ut,ub);

boxjoin()
boxit.ta("Typed Capability 1");
ta.ne = ub.sw-(10bp, 30bp);
drawboxed(ta);

boxjoin(a.ne = b.nw; a.se = b.sw);
boxit.tat("Type");
boxit.tab("Badge");
boxit.tar("Rights");
tat.nw = ta.sw;
tar.ne = ta.se;
drawboxed(tat,tab,tar);

boxjoin()
boxit.tb("Typed Capability 2");
tb.nw = ta.ne+(10bp, 0bp);
drawboxed(tb);

boxjoin(a.ne = b.nw; a.se = b.sw);
boxit.tbt("Type");
boxit.tbb("Badge");
boxit.tbr("Rights");
tbt.nw = tb.sw;
tbr.ne = tb.se;
drawboxed(tbt,tbb,tbr);

boxjoin()
boxit.tn("Typed Capability n");
tn.nw = tb.ne+(50bp, 0bp);
drawboxed(tn);

boxjoin(a.ne = b.nw; a.se = b.sw);
boxit.tnt("Type");
boxit.tnb("Badge");
boxit.tnr("Rights");
tnt.nw = tn.sw;
tnr.ne = tn.se;
drawboxed(tnt,tnb,tnr);

drawarrow (center (ub.s--ub.sw))--ta.n;
drawarrow ub.s--tb.n;
drawarrow (center (ub.s--ub.se))--tn.n;
draw tb.e--tn.w dashed withdots;

endfig;
end.

