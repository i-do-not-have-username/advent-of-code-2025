import java.util.*;

public class Solution01 {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        List<String> grid = new ArrayList<>();
        
        while (scanner.hasNextLine()) {
            String line = scanner.nextLine();
            grid.add(line);
        }
        scanner.close();
        
        // Find starting position S
        int startRow = -1, startCol = -1;
        for (int r = 0; r < grid.size(); r++) {
            for (int c = 0; c < grid.get(r).length(); c++) {
                if (grid.get(r).charAt(c) == 'S') {
                    startRow = r;
                    startCol = c;
                    break;
                }
            }
            if (startRow != -1) break;
        }
        
        int splits = countSplits(grid, startRow, startCol);
        System.out.println(splits);
    }
    
    static int countSplits(List<String> grid, int startRow, int startCol) {
        int rows = grid.size();
        
        // Queue for BFS - each element is [row, col] representing a beam position
        Queue<int[]> queue = new LinkedList<>();
        // Track which (row, col) beam positions we've already added to queue
        Set<String> processedBeams = new HashSet<>();
        // Track which splitters have been hit
        Set<String> hitSplitters = new HashSet<>();
        
        queue.offer(new int[]{startRow, startCol});
        processedBeams.add(startRow + "," + startCol);
        
        while (!queue.isEmpty()) {
            int[] pos = queue.poll();
            int row = pos[0];
            int col = pos[1];
            
            // Move this beam downward until it hits a splitter or exits
            for (int r = row + 1; r < rows; r++) {
                if (col < 0 || col >= grid.get(r).length()) break;
                
                char cell = grid.get(r).charAt(col);
                
                if (cell == '^') {
                    // Check if we've already hit this splitter
                    String splitterKey = r + "," + col;
                    if (!hitSplitters.contains(splitterKey)) {
                        hitSplitters.add(splitterKey);
                    }
                    
                    // Create beam to the left of splitter
                    if (col - 1 >= 0) {
                        String leftKey = r + "," + (col - 1);
                        if (!processedBeams.contains(leftKey)) {
                            processedBeams.add(leftKey);
                            queue.offer(new int[]{r, col - 1});
                        }
                    }
                    
                    // Create beam to the right of splitter
                    String rightKey = r + "," + (col + 1);
                    if (!processedBeams.contains(rightKey)) {
                        processedBeams.add(rightKey);
                        queue.offer(new int[]{r, col + 1});
                    }
                    
                    break; // This beam stops here
                }
            }
        }
        
        return hitSplitters.size();
    }
}
