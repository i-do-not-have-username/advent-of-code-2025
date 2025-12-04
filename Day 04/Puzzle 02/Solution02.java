import java.util.Scanner;
import java.util.ArrayList;
import java.util.List;

public class Solution02 {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        
        // Read the grid (use char arrays so we can modify)
        List<char[]> grid = new ArrayList<>();
        while (scanner.hasNextLine()) {
            String line = scanner.nextLine();
            if (line.isEmpty()) break;
            grid.add(line.toCharArray());
        }
        scanner.close();
        
        int totalRemoved = removeAllAccessibleRolls(grid);
        System.out.println(totalRemoved);
    }
    
    private static int removeAllAccessibleRolls(List<char[]> grid) {
        int totalRemoved = 0;
        boolean removedAny = true;
        
        // Keep removing until no more rolls can be removed
        while (removedAny) {
            removedAny = false;
            int rows = grid.size();
            if (rows == 0) break;
            int cols = grid.get(0).length;
            
            // Find all accessible rolls in this iteration
            List<int[]> toRemove = new ArrayList<>();
            
            for (int row = 0; row < rows; row++) {
                for (int col = 0; col < cols; col++) {
                    if (grid.get(row)[col] == '@') {
                        int adjacentRolls = countAdjacentRolls(grid, row, col, rows, cols);
                        if (adjacentRolls < 4) {
                            toRemove.add(new int[]{row, col});
                        }
                    }
                }
            }
            
            // Remove all accessible rolls at once
            for (int[] pos : toRemove) {
                grid.get(pos[0])[pos[1]] = '.';
                totalRemoved++;
                removedAny = true;
            }
        }
        
        return totalRemoved;
    }
    
    private static int countAdjacentRolls(List<char[]> grid, int row, int col, int rows, int cols) {
        // 8 directions: up, down, left, right, and 4 diagonals
        int[] dRow = {-1, -1, -1, 0, 0, 1, 1, 1};
        int[] dCol = {-1, 0, 1, -1, 1, -1, 0, 1};
        
        int count = 0;
        
        for (int i = 0; i < 8; i++) {
            int newRow = row + dRow[i];
            int newCol = col + dCol[i];
            
            // Check if the position is within bounds
            if (newRow >= 0 && newRow < rows && newCol >= 0 && newCol < cols) {
                // Check if there's a roll at this position
                if (grid.get(newRow)[newCol] == '@') {
                    count++;
                }
            }
        }
        
        return count;
    }
}
