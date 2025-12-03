import java.util.Scanner;

public class Solution02 {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        
        long totalJoltage = 0;
        
        while (scanner.hasNextLine()) {
            String bank = scanner.nextLine().trim();
            if (bank.isEmpty()) continue;
            
            String maxJoltage = findMaxJoltage(bank);
            totalJoltage += Long.parseLong(maxJoltage);
        }
        
        scanner.close();
        System.out.println(totalJoltage);
    }
    
    // Find the maximum 12-digit joltage for a single bank
    private static String findMaxJoltage(String bank) {
        StringBuilder result = new StringBuilder();
        int currentPos = 0;
        int bankLength = bank.length();
        
        // We need to select exactly 12 digits
        for (int i = 0; i < 12; i++) {
            int remaining = 12 - i;  // How many more digits we need
            
            // We can look from currentPos up to (bankLength - remaining)
            // This ensures we have enough digits left to complete our selection
            int maxEndPos = bankLength - remaining;
            
            // Find the maximum digit in the valid range
            char maxDigit = '0';
            int maxPos = currentPos;
            
            for (int j = currentPos; j <= maxEndPos; j++) {
                if (bank.charAt(j) > maxDigit) {
                    maxDigit = bank.charAt(j);
                    maxPos = j;
                }
            }
            
            // Add the maximum digit to result
            result.append(maxDigit);
            
            // Move to the position after the selected digit
            currentPos = maxPos + 1;
        }
        
        return result.toString();
    }
}
