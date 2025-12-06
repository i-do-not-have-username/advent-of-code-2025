import java.util.Scanner;
import java.util.ArrayList;
import java.util.List;

public class Solution02 {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        
        // Read all lines
        List<String> lines = new ArrayList<>();
        while (scanner.hasNextLine()) {
            lines.add(scanner.nextLine());
        }
        scanner.close();
        
        if (lines.isEmpty()) {
            System.out.println(0);
            return;
        }
        
        // Find the maximum width
        int maxWidth = 0;
        for (String line : lines) {
            maxWidth = Math.max(maxWidth, line.length());
        }
        
        // Pad all lines to the same width
        for (int i = 0; i < lines.size(); i++) {
            String line = lines.get(i);
            while (line.length() < maxWidth) {
                line += " ";
            }
            lines.set(i, line);
        }
        
        // Process from RIGHT to LEFT
        List<Problem> problems = new ArrayList<>();
        int col = maxWidth - 1;
        
        while (col >= 0) {
            // Skip separator columns (all spaces)
            while (col >= 0 && isEmptyColumn(lines, col)) {
                col--;
            }
            
            if (col < 0) break;
            
            // Found end of a problem (reading right to left), find its start
            int end = col;
            while (col >= 0 && !isEmptyColumn(lines, col)) {
                col--;
            }
            int start = col + 1;
            
            // Extract the problem from these columns (start to end)
            problems.add(extractProblem(lines, start, end));
        }
        
        // Calculate grand total
        long grandTotal = 0;
        for (Problem p : problems) {
            grandTotal += p.solve();
        }
        
        System.out.println(grandTotal);
    }
    
    // Check if a column is entirely spaces
    static boolean isEmptyColumn(List<String> lines, int col) {
        for (String line : lines) {
            if (col < line.length() && line.charAt(col) != ' ') {
                return false;
            }
        }
        return true;
    }
    
    // Extract a problem from the given column range, reading RIGHT to LEFT
    static Problem extractProblem(List<String> lines, int start, int end) {
        List<Long> numbers = new ArrayList<>();
        char operation = '+';
        int lastRow = lines.size() - 1;
        
        // Process columns from RIGHT to LEFT (end to start)
        for (int col = end; col >= start; col--) {
            // Find the operator in this problem (in the last row)
            char lastChar = lines.get(lastRow).charAt(col);
            if (lastChar == '+' || lastChar == '*') {
                operation = lastChar;
            }
            
            // Extract the number from this column (reading top to bottom)
            StringBuilder numStr = new StringBuilder();
            for (int row = 0; row < lastRow; row++) {  // Exclude the operator row
                char c = lines.get(row).charAt(col);
                if (c >= '0' && c <= '9') {
                    numStr.append(c);
                }
            }
            
            // If we found digits, parse as a number
            if (numStr.length() > 0) {
                numbers.add(Long.parseLong(numStr.toString()));
            }
        }
        
        return new Problem(numbers, operation);
    }
    
    static class Problem {
        List<Long> numbers;
        char operation;
        
        Problem(List<Long> numbers, char operation) {
            this.numbers = numbers;
            this.operation = operation;
        }
        
        long solve() {
            if (numbers.isEmpty()) return 0;
            
            long result = numbers.get(0);
            for (int i = 1; i < numbers.size(); i++) {
                if (operation == '+') {
                    result += numbers.get(i);
                } else if (operation == '*') {
                    result *= numbers.get(i);
                }
            }
            return result;
        }
    }
}
