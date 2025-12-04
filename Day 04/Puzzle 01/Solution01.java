import java.util.Scanner;
import java.util.ArrayList;
import java.util.List;

public class Solution01 {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        
        // Read the grid
        List<String> grid = new ArrayList<>();
        while (scanner.hasNextLine()) {
            String line = scanner.nextLine();
            if (line.isEmpty()) break;
            grid.add(line);
        }
        scanner.close();
        
        int accessibleCount = countAccessibleRolls(grid);
        System.out.println(accessibleCount);
    }
    
    private static int countAccessibleRolls(List<String> grid) {
        int rows = grid.size();
        if (rows == 0) return 0;
        int cols = grid.get(0).length();
        
        int accessibleCount = 0;
        
        // Check each position in the grid
        for (int row = 0; row < rows; row++) {
            for (int col = 0; col < cols; col++) {
                // If this position has a roll of paper
                if (grid.get(row).charAt(col) == '@') {
                    // Count adjacent rolls
                    int adjacentRolls = countAdjacentRolls(grid, row, col, rows, cols);
                    
                    // If fewer than 4 adjacent rolls, it's accessible
                    if (adjacentRolls < 4) {
                        accessibleCount++;
                    }
                }
            }
        }
        
        return accessibleCount;
    }
    
    private static int countAdjacentRolls(List<String> grid, int row, int col, int rows, int cols) {
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
                if (grid.get(newRow).charAt(newCol) == '@') {
                    count++;
                }
            }
        }
        
        return count;
    }
}
