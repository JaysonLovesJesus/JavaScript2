// Importing the 'math' module from npm
from "npm:mathjs@10.0.0" import { add, subtract };

// Defining a constant
const PI = 3.14159;

// Main function
pub fn main() {
    // Using an unsafe block for an imported function that may throw
    unsafe {
        let result = add(3, 5);
        console.log("3 + 5 =", result);
    }

    // Using try-catch for error handling
    let area = calculateCircleArea(5) catch |error| {
        return console.error("Error calculating area:", error);
    };
    console.log("Area of circle with radius 5:", area);
}

// Function to calculate the area of a circle
fn calculateCircleArea(radius: number) -> number {
    return PI * radius * radius;
}
