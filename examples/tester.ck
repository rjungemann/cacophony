OscIn oin;
OscMsg msg;
13698 => oin.port;
oin.addAddress("/status");

OscOut oout;
oout.dest("127.0.0.1", 13699);

while (true) {
  oin => now;
  while (oin.recv(msg)) {
    <<< "Received: /status" >>>;
    oout.start("/status");
    oout.send();
  }
}
