import java.util.Scanner;
import java.util.ArrayList;
import java.util.List;

public class Solution01 {
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
        
        // Pad all lines to the same width for easier processing
        for (int i = 0; i < lines.size(); i++) {
            String line = lines.get(i);
            while (line.length() < maxWidth) {
                line += " ";
            }
            lines.set(i, line);
        }
        
        // Identify problem boundaries
        List<Problem> problems = new ArrayList<>();
        int col = 0;
        
        while (col < maxWidth) {
            // Skip separator columns (all spaces)
            while (col < maxWidth && isEmptyColumn(lines, col)) {
                col++;
            }
            
            if (col >= maxWidth) break;
            
            // Found start of a problem, find its end
            int start = col;
            while (col < maxWidth && !isEmptyColumn(lines, col)) {
                col++;
            }
            int end = col - 1;
            
            // Extract the problem from these columns
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
    
    // Extract a problem from the given column range
    static Problem extractProblem(List<String> lines, int start, int end) {
        List<Long> numbers = new ArrayList<>();
        char operation = '+';
        
        for (String line : lines) {
            // Get the substring for this problem
            String segment = line.substring(start, end + 1).trim();
            
            if (segment.isEmpty()) continue;
            
            // Check if it's an operation
            if (segment.equals("+") || segment.equals("*")) {
                operation = segment.charAt(0);
            } else {
                // Try to parse as a number
                try {
                    numbers.add(Long.parseLong(segment));
                } catch (NumberFormatException e) {
                    // Skip if not a valid number
                }
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
