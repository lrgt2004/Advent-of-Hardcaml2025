
## Solutions
### Day1 Part1 + Part2
I take the most stupid solution here, which is simulating the tick through every request, and count whenever it's zero.
### Day3 Part1 + Part2
Greedily select the largest number, but there is no doubt that I need to ensure that the remaining number is enough for the next selection. This cycle continues until all **k** digits are selected.
### Day4 Part1 + Part2
Use ram to store the entire matrix, and check whether the neighbor is **@** through multiple read_ports. Part1 only needs to confirm accumulatively after one side of the cycle, while part2 will set the grids that meet the conditions as **.** In this way. If the count does not increase after one cycle, that result is correct. At the same time, in order to reduce boundary judgment, I manually added an additional layer on the boundary.
### Day5 Part1 + Part2
Part1 is very easy as I just need to go through ID ranges and check whether given ID is in that range. Part2 requires me to maintain a range, dymatically change it when going through ID ranges and adding the range length to the answer.
### Day7 Part1 + Part2
Day7 is just like Day4. Still, use ram to store the entire matrix, and start from the first line. Use an additional ram to record the number of beams in each row. If **^** is encountered, split the beams on both sides (because **^** will not appear continuously, so this is correct) and accumulate them. Count them in the last row, so that Part1 and part2 can be completed at the same time.
### Day9 Part1
Stores all coordinates in ram and continually calculates the max area between any 2 coordinates.
### Day10 Part1
Because the minimum number of times in Part1 is required to press each button at most once. Therefore, other lights affected by each button can be converted into binary form first. Then use binary number to simulate DFS, check each bit of 他the binary number, if it's **1**, XOR the corresponding binary number, finally check whether it is consistent with the result, and take min for the answer.
### Day12
A pure fitting scheme that multiplies the sum of all the present areas given by 1.3. If it is less than or equal to the region area, then the counter plus one.
