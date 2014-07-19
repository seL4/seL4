input boxes;
beginfig(1);

defaultfont := "ptmr8r";

% Client
boxjoin(a.sw = b.nw + (0bp, 30bp); a.se = b.ne + (0bp, 30bp));
boxit.coc("Object Cap");
fixsize(coc);
boxit.crc("Reply Cap");
drawboxed(coc, crc);
boxjoin();
circleit.sw("Call"); sw.dx = sw.dy;
sw.e = center(coc.w -- crc.w) - (10bp, 0bp);
drawboxed(sw);
boxit.client();
client.w = sw.w - (45bp, 0bp);
client.ne = coc.ne + (20bp, 20bp);
fixpos(client);
fill bpath(client) withcolor 0.7white;
unfill bpath(coc);
unfill bpath(crc);
unfill bpath(sw);
drawboxed(coc, crc, sw);
label.top("Client", client.n);

% Kernel
boxjoin(a.sw = b.nw + (0bp, 30bp); a.se = b.ne + (0bp, 30bp));
boxit.ko("Object");
ko.w = coc.e + (96bp, 0bp);
boxit.kep("Endpoint");
fixsize(kep);
drawboxed(ko, kep);
boxjoin();
circleit.sc("Syscall"); sc.dx = sc.dy;
sc.w = center(ko.e -- kep.e) + (20bp, 0bp);
drawboxed(sc);
boxit.kernel();
kernel.e = sc.e + (20bp, 0bp);
kernel.sw = kep.sw - (20bp, 20bp);
fixpos(kernel);
fill bpath(kernel) withcolor 0.7white;
unfill bpath.ko;
unfill bpath.kep;
unfill bpath.sc;
drawboxed(ko, kep, sc);
label.top("Kernel", kernel.n);

% Operation
drawarrow sw.c..coc.w{right} cutbefore bpath.sw dashed evenly;
drawarrow coc.e--ko.w dashed evenly;
drawarrow ko.e{right}..sc.c cutafter bpath.sc dashed evenly;
drawarrow sc.c..kep.e{left} cutbefore bpath.sc dashed evenly;
drawarrow kep.w--crc.e dashed evenly;
drawarrow crc.w{left}..sw.c cutafter bpath.sw dashed evenly;

pair req, res;
req = sw.c - (15bp, -15bp);
label.lft("Request", req);
res = sw.c - (15bp, 15bp);
label.lft("Result", res);
drawarrow req{right}..sw.c cutafter bpath.sw dashed evenly;
drawarrow sw.c..res{left} cutbefore bpath.sw dashed evenly;

endfig;
end.

