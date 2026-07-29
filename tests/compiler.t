Factorial tests

  $ cat << 'EOF' > fac.txt
  > let fac n = if n < 2 then 1 else fac (n - 1) * n
  > let main = fac 5
  > EOF

  $ ../src/main.exe fac.txt fac.S
  > riscv64-linux-gnu-as -march=rv64gc fac.S -o temp.o
  > riscv64-linux-gnu-ld temp.o -o file.exe
  > qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./file.exe
  [120]

  $ cat << 'EOF' > fac.txt
  > let fac n = if n < 2 then 1 else fac (n - 1) * n
  > let main = fac 4
  > EOF

  $ ../src/main.exe fac.txt fac.S
  > riscv64-linux-gnu-as -march=rv64gc fac.S -o temp.o
  > riscv64-linux-gnu-ld temp.o -o file.exe
  > qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./file.exe
  [24]

Fibonacci tests

  $ cat << 'EOF' > fib.txt
  > let fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)
  > let main = fib 5
  > EOF

  $ ../src/main.exe fib.txt fib.S
  > riscv64-linux-gnu-as -march=rv64gc fib.S -o temp.o
  > riscv64-linux-gnu-ld temp.o -o file.exe
  > qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./file.exe
  [5]

  $ cat << 'EOF' > fib.txt
  > let fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)
  > let main = fib 4
  > EOF

  $ ../src/main.exe fib.txt fib.S
  > riscv64-linux-gnu-as -march=rv64gc fib.S -o temp.o
  > riscv64-linux-gnu-ld temp.o -o file.exe
  > qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./file.exe
  [3]

Mccarthy91 tests

  $ cat << 'EOF' > mcc.txt
  > let mcc n = if 100 < n then n - 10 else mcc (mcc (n + 11))
  > let main = mcc 5
  > EOF

  $ ../src/main.exe mcc.txt mcc.S
  > riscv64-linux-gnu-as -march=rv64gc mcc.S -o temp.o
  > riscv64-linux-gnu-ld temp.o -o file.exe
  > qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./file.exe
  [91]

  $ cat << 'EOF' > mcc.txt
  > let mcc n = if 100 < n then n - 10 else mcc (mcc (n + 11))
  > let main = mcc 42
  > EOF

  $ ../src/main.exe mcc.txt mcc.S
  > riscv64-linux-gnu-as -march=rv64gc mcc.S -o temp.o
  > riscv64-linux-gnu-ld temp.o -o file.exe
  > qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./file.exe
  [91]
