input boxes;
beginfig(1);

defaultfont := "ptmr8r";

% Client
boxjoin(a.sw = b.nw + (0bp, 30bp); a.se = b.ne + (0bp, 30bp));
boxit.coc("Service Cap");
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
boxit.ko("Service Cap");
fixsize(ko);
ko.w = coc.e + (96bp, 0bp);
boxit.kep("Reply Cap");
drawboxed(ko, kep);
boxjoin();
circleit.send("Reply"); send.dx = send.dy;
send.w = kep.e + (15bp, 0bp);
drawboxed(send);
circleit.wait("Wait"); wait.dx = wait.dy;
wait.c = (xpart send.c, ypart ko.c);
drawboxed(wait);
boxit.kernel();
kernel.e = center(wait.e--send.e) + (20bp, 0bp);
kernel.sw = kep.sw - (20bp, 20bp);
fixpos(kernel);
fill bpath(kernel) withcolor 0.7white;
unfill bpath.ko;
unfill bpath.kep;
unfill bpath.wait;
unfill bpath.send;
drawboxed(ko, kep, wait, send);
label.top("Server", kernel.n);

% Operation
drawarrow sw.c..coc.w{right} cutbefore bpath.sw dashed evenly;
drawarrow coc.e--ko.w dashed evenly;
drawarrow ko.e--wait.w dashed evenly;
drawarrow send.w--kep.e dashed evenly;
drawarrow kep.w--crc.e dashed evenly;
drawarrow crc.w{left}..sw.c cutafter bpath.sw dashed evenly;

pair req, res;
req = sw.c - (15bp, -15bp);
label.lft("Request", req);
res = sw.c - (15bp, 15bp);
label.lft("Result", res);
drawarrow req{right}..sw.c cutafter bpath.sw dashed evenly;
drawarrow sw.c..res{left} cutbefore bpath.sw dashed evenly;

boxit.op("Operation");
op.c = center(send.c -- wait.c);
drawunboxed(op);
drawarrow wait.c -- op.n cutbefore bpath.wait dashed evenly;
drawarrow op.s -- send.c cutafter bpath.send dashed evenly;

endfig;
end.

