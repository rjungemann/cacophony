1 => int running;
OscIn oin;
OscMsg msg;
Std.atoi(me.arg(0)) => oin.port;
oin.addAddress("/quit,N");
oin.addAddress("/machine/status,N");
oin.addAddress("/machine/shreds,N");
oin.addAddress("/machine/add,s");
oin.addAddress("/machine/remove,i");
oin.addAddress("/machine/replace,is");

chout <= "[engine] running" <= IO.newline();

while (running != 0) {
  oin => now;
  while (oin.recv(msg) != 0) {
    if (msg.address == "/quit") {
      0 => running;
    }

    if (msg.address == "/machine/status") {
      Machine.status() => int n;
      chout <= "[engine] " <= n <= IO.newline();
    }

    if (msg.address == "/machine/shreds") {
      Machine.shreds() @=> int shreds[];
      chout <= "[engine] [";
      for (0 => int i; i < shreds.size(); i++) {
        chout <= shreds[i];
        if (i < shreds.size() - 1) {
          chout <= ", ";
        }
      }
      chout <= "]" <= IO.newline();
    }

    if (msg.address == "/machine/add") {
      chout <= "[engine] " <= Machine.add(msg.getString(0)) <= IO.newline();
    }

    if (msg.address == "/machine/remove") {
      chout <= "[engine] " <= Machine.remove(msg.getInt(0)) <= IO.newline();
    }

    if (msg.address == "/machine/replace") {
      chout <= "[engine] " <= Machine.replace(msg.getInt(0), msg.getString(1)) <= IO.newline();
    }
  }
}

chout <= "[engine] quit" <= IO.newline();
