input = 368078

# radius spiraly
qty = 0
sum = 0
while sum < input:
  qty += 8
  sum += qty
radius = int(qty/8)
  
# predem znamy pocet iteraci
iterations = 0
qty = 0
sum = 0
while sum < input:
  iterations += 1
  qty += 2
  sum += qty
  
def spiral_sum_neighbours(spirale, x, y, bounds):
  sum = 0
  #print("["+str(x)+"]["+str(y)+"]")
  for x_offset, y_offset in [(-1, -1), (-1, 0), (-1, 1),
                             ( 0, -1),          ( 0, 1),
                             ( 1, -1), ( 1, 0), ( 1, 1)]:
      if x+x_offset<0 or y+y_offset<0 or x+x_offset>bounds or y+y_offset>bounds:
        continue
      sum += spirale[x+x_offset][y+y_offset]
      #print(sum, "(+"+str(spirale[x+x_offset][y+y_offset])+") ["+str(x+x_offset)+"]["+str(y+y_offset)+"]")
  #print('\n'.join([''.join(['{:4}'.format(item) for item in row]) for row in spirale]), "\n")
  return sum

def spiral_sum(iterations, min_sum):
  xpos = radius
  ypos = radius
  steps = 1
  
  spirale = [[0 for x in range(radius*2+1)] for y in range(radius*2+1)]
  spirale[xpos][ypos] = 1 # put 1 in center 
  
  sum = 0
  for iteration in range(iterations):
    direction = -1 if iteration % 2 else 1
    
    for y in range(steps):
      ypos += direction
      sum = spiral_sum_neighbours(spirale, xpos, ypos, radius*2)
      spirale[xpos][ypos] = sum
      if(sum > min_sum):
        return sum
      
    for x in range(steps):
      xpos += direction
      sum = spiral_sum_neighbours(spirale, xpos, ypos, radius*2)
      spirale[xpos][ypos] = sum
      if(sum > min_sum):
        return sum
        
    # ocislovany jsou pruchody tema dvema forama nad timto komentarem (12 pruchodu forama, 6 iteraci)
    # (spirala je otocena o 90 stupnu doprava oproti zadani kvuli jednoduchosti)
    # 
    # 12 12 12 12 12 12 11
    #  .  8  8  8  8  7 11
    #  .  9  4  4  3  7 11
    #  .  9  5  X  3  7 11
    #  .  9  5  1  2  7 11
    #  .  9  5  6  6  6 11
    #  .  9 10 10 10 10 10
    #  
    
    steps += 1

print( spiral_sum(iterations, input) )
#print('\n'.join([''.join(['{:6}'.format(item) for item in row]) for row in spirale]), "\n")
