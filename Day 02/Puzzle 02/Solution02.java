import java.util.Scanner;

public class Solution02 {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        
        // Read the entire input (could be multiple lines)
        StringBuilder input = new StringBuilder();
        while (scanner.hasNextLine()) {
            input.append(scanner.nextLine().trim());
        }
        scanner.close();
        
        // Parse ranges separated by commas
        String[] ranges = input.toString().split(",");
        
        long totalSum = 0;
        
        for (String range : ranges) {
            range = range.trim();
            if (range.isEmpty()) continue;
            
            // Parse start and end of range
            String[] parts = range.split("-");
            long start = Long.parseLong(parts[0]);
            long end = Long.parseLong(parts[1]);
            
            // Check each ID in the range
            for (long id = start; id <= end; id++) {
                if (isInvalid(id)) {
                    totalSum += id;
                }
            }
        }
        
        System.out.println(totalSum);
    }
    
    // Check if an ID is invalid (repeated pattern at least twice)
    private static boolean isInvalid(long id) {
        String str = String.valueOf(id);
        int len = str.length();
        
        // Try all possible pattern lengths from 1 to len/2
        for (int patternLen = 1; patternLen <= len / 2; patternLen++) {
            // Check if the length is divisible by pattern length
            if (len % patternLen == 0) {
                String pattern = str.substring(0, patternLen);
                boolean matches = true;
                
                // Check if the pattern repeats throughout the entire string
                for (int i = patternLen; i < len; i += patternLen) {
                    if (!str.substring(i, i + patternLen).equals(pattern)) {
                        matches = false;
                        break;
                    }
                }
                
                if (matches) {
                    return true;
                }
            }
        }
        
        return false;
    }
}
