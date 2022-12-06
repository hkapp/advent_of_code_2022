with open("task1.csv", "r") as f:
  s = f.readline()
  my_max = 0
  while s:
    sum = 0
    while s and s != '\n':
      n = int(s)
      sum += n
      s = f.readline()
    print(sum)
    my_max = max(my_max, sum)
    s = f.readline()
  print('Max elf: ' + str(my_max))
