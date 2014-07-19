input boxes;
beginfig(1);

defaultfont := "ptmr8r";

% Endpoint Capability
def epcap(expr badgename) = begingroup
    save cap, badge;
    picture cap;
    cap = thelabel(badgename,(0,0));
    addto cap doublepath bbox cap;
    addto cap also thelabel.top("Endpoint Cap", center(ulcorner cap--urcorner cap));
    cap endgroup;
enddef;

% Sender TCB
boxjoin(b.nw = a.sw; b.ne = a.se);
boxit.sa();
sa.s = sa.n - (0bp, 20bp);
boxit.sb("CSpace Root");
boxit.sc();
sc.s = sc.n - (0bp, 20bp);
boxit.sd("Register State");
fixsize(sd);
drawboxed(sa, sb, sc, sd);
label.top("Sender TCB", sa.n);

% Sender CSpace
boxjoin(a.sw = b.nw; a.se = b.ne);
boxit.se();
se.n = sd.s - (0bp, 30bp);
se.s = se.n - (0bp, 50bp);
picture scap;
scap = epcap("Sender Badge");
boxit.sf(scap);
fixsize(sf);
boxit.sg();
sg.s = sg.n - (0bp, 30bp);
drawboxed(se, sf, sg);
label.top("Sender CSpace", se.n);
drawarrow(sb.w{left} .. se.nw{dir 315});

% Send System Call
pair ssc;
ssc = sf.w - (36bp, 0bp);
drawarrow ssc--sf.w dashed withdots;
label.lft("Send", ssc);

% Recipient TCB
boxjoin(b.nw = a.sw; b.ne = a.se);
boxit.ra();
ra.nw = sa.ne + (144bp, 0bp);
ra.s = ra.n - (0bp, 20bp);
boxit.rb("CSpace Root");
boxit.rc();
rc.s = rc.n - (0bp, 20bp);
boxit.rd("Register State");
fixsize(rd);
drawboxed(ra, rb, rc, rd);
label.top("Recipient TCB", ra.n);

% Recipient CSpace
boxjoin(a.sw = b.nw; a.se = b.ne);
boxit.re();
re.n = rd.s - (0bp, 30bp);
re.s = re.n - (0bp, 20bp);
picture rcap;
rcap = epcap("Unused Badge");
boxit.rf(rcap);
fixsize(rf);
boxit.rg();
rg.s = rg.n - (0bp, 60bp);
drawboxed(re, rf, rg);
label.top("Recipient CSpace", re.n);
drawarrow(rb.e{right} .. re.ne{dir 225});

% Wait System Call
pair wsc;
wsc = rf.e + (36bp, 0bp);
drawarrow wsc--rf.e dashed withdots;
label.rt("Wait", wsc);

% Endpoint
boxjoin();
boxit.ep("Endpoint");
ep.c = center(sf.e -- rf.w);
drawboxed(ep);
drawarrow(sf.e -- ep.w);
drawarrow(rf.w -- ep.e);

% Message
boxjoin(a.sw = b.nw; a.se = b.ne);
boxit.mrs("Message Registers");
mrs.c = center(sd.e -- rd.w);
boxit.badge("Badge");
drawboxed(mrs, badge);
label.top("Message", mrs.n);
drawarrow(sd.e -- mrs.w) dashed evenly;
drawarrow(sf.e -- badge.w) dashed evenly;
drawarrow(mrs.se -- rd.w) dashed evenly;

endfig;
end.

