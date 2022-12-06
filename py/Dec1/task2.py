with open("task1.csv", "r") as f:
  s = f.readline()
  my_cals = []
  while s:
    my_sum = 0
    while s and s != '\n':
      n = int(s)
      my_sum += n
      s = f.readline()
    print(my_sum)
    my_cals.append(my_sum)
    s = f.readline()
  my_cals.sort()
  top3 = my_cals[-3:]
  print('Top 3: ' + str(top3))
  print('Sum top 3: ' + str(sum(top3)))
