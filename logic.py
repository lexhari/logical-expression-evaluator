import re
from itertools import product
import argparse
import sys

# Scanner class to tokenize the input string based on predefined patterns
class Scanner:
    def __init__(self):
        self.token_patterns = [
            (r"TRUE|FALSE", "BOOLEAN"),  # Match boolean literals
            (r"P|Q|S", "VARIABLE"),      # Match variables P, Q, S
            (r"\(", "LPAREN"),         # Match left parenthesis
            (r"\)", "RPAREN"),         # Match right parenthesis
            (r"NOT", "NOT"),             # Match NOT operator
            (r"AND", "AND"),             # Match AND operator
            (r"OR", "OR"),               # Match OR operator
            (r"IMPLIES", "IMPLIES"),     # Match IMPLIES operator
            (r"EQUIVALENT", "EQUIVALENT"), # Match EQUIVALENT operator
            (r"\s+", None),            # Ignore whitespace
        ]
        self.regex = [(re.compile(pattern), token_type) for pattern, token_type in self.token_patterns]

    # Scan the input string and return a list of tokens
    def scan(self, input_string):
        tokens = []
        position = 0
        while position < len(input_string):
            match_found = False
            for pattern, token_type in self.regex:
                match = pattern.match(input_string, position)
                if match:
                    match_found = True
                    text = match.group(0)
                    if token_type:
                        tokens.append((token_type, text))
                    position = match.end()
                    break
            if not match_found:
                raise ValueError(f"Unexpected character at position {position}: '{input_string[position]}'")
        return tokens

# Iterator class to traverse the list of tokens
class Iterator:
    def __init__(self, token_list):
        self._list = token_list
        self._length = len(token_list)
        self._currentIndex = 0

    # Retrieve the next token
    def next(self):
        if self._currentIndex < self._length:
            self._currentIndex += 1
            return self._list[self._currentIndex - 1]
        else:
            return None

    # Peek at the current token without moving the index
    def peek(self):
        if self._currentIndex < self._length:
            return self._list[self._currentIndex]
        else:
            return None

# Parser class to construct an Abstract Syntax Tree (AST) from tokens
class Parser:
    def __init__(self):
        self.errors = []

    # Add an error message to the list of errors
    def add_error(self, message, token=None):
        if token:
            self.errors.append(f"{message} at '{token[1]}'")
        else:
            self.errors.append(message)

    # Parse a sentence (the main entry point for parsing)
    def Sentence(self, myIter):
        token = myIter.peek()

        # Handle atomic sentences (BOOLEAN or VARIABLE)
        if token and token[0] in {"BOOLEAN", "VARIABLE"}:
            left = self.AtomicSentence(myIter)

            # Handle logical connectives (AND, OR, IMPLIES, EQUIVALENT)
            while myIter.peek() and myIter.peek()[0] in {"AND", "OR", "IMPLIES", "EQUIVALENT"}:
                connective_token = myIter.next()
                right = self.Sentence(myIter)
                if not right:
                    self.add_error("Invalid sentence after connective", connective_token)
                    return None
                left = (connective_token[0], left, right)

            if myIter.peek() and myIter.peek()[0] in {"BOOLEAN", "VARIABLE"}:
                self.add_error("Missing connective between atomic sentences", myIter.peek())
                return None

            return left

        # Handle parentheses-enclosed expressions
        elif token and token[0] == "LPAREN":
            myIter.next()
            expr = self.Sentence(myIter)
            if not expr:
                self.add_error("Invalid sentence after '('")
                return None

            if myIter.peek() and myIter.peek()[0] == "RPAREN":
                myIter.next()
                if myIter.peek() and myIter.peek()[0] in {"AND", "OR", "IMPLIES", "EQUIVALENT"}:
                    connective_token = myIter.next()
                    right = self.Sentence(myIter)
                    if not right:
                        self.add_error("Invalid sentence after connective", connective_token)
                        return None
                    return (connective_token[0], expr, right)

                return expr

            else:
                self.add_error("Missing closing parenthesis", token)
                return None

        # Handle NOT operator
        elif token and token[0] == "NOT":
            myIter.next()
            operand = self.Sentence(myIter)
            if not operand:
                self.add_error("Invalid sentence after 'NOT'", myIter.peek())
                return None
            return ("NOT", operand)

        # Handle unexpected tokens
        else:
            self.add_error("Unexpected token", token)
            return None

    # Parse an atomic sentence (BOOLEAN or VARIABLE)
    def AtomicSentence(self, iterator):
        token = iterator.peek()
        if token and token[0] in {"BOOLEAN", "VARIABLE"}:
            return iterator.next()

# Evaluator class to evaluate the AST and generate truth tables
class Evaluator:
    def __init__(self):
        self.root = None

    def evaluate(self, node, values):
        if not node:
            return None
        if node[0] == "BOOLEAN":
            return node[1] == "TRUE"
        elif node[0] == "VARIABLE":
            return values[node[1]]
        elif node[0] == "NOT":
            return not self.evaluate(node[1], values)
        elif node[0] in {"AND", "OR", "IMPLIES", "EQUIVALENT"}:
            left_result = self.evaluate(node[1], values)
            right_result = self.evaluate(node[2], values)
            if node[0] == "AND":
                return left_result and right_result
            elif node[0] == "OR":
                return left_result or right_result
            elif node[0] == "IMPLIES":
                return not left_result or right_result
            elif node[0] == "EQUIVALENT":
                return left_result == right_result

    def generate_truth_table(self, root, variables):
        self.root = root
        table = []
        variables = sorted(variables)

        # Collect unique subexpressions
        subexpressions = self.collect_subexpressions(root)

        # Evaluate the expression for all possible variable combinations
        for combo in product([False, True], repeat=len(variables)):
            values = dict(zip(variables, combo))
            result = self.evaluate(root, values)
            subresults = {expr: self.evaluate(expr, values) for expr in subexpressions}
            table.append((values, result, subresults))

        return table

    # Collect all subexpressions of the AST
    def collect_subexpressions(self, node):
        if not node:
            return set()
        subexpressions = set()
        if node[0] in {"BOOLEAN", "VARIABLE"}:
            return subexpressions
        elif node[0] == "NOT":
            subexpressions.add(node)
            subexpressions.update(self.collect_subexpressions(node[1]))
        else:  # Connectives
            subexpressions.add(node)
            subexpressions.update(self.collect_subexpressions(node[1]))
            subexpressions.update(self.collect_subexpressions(node[2]))
        return subexpressions - {self.root}  # Exclude the root expression

    # Convert a subexpression node to a human-readable string
    def print_subexpression(self, node):
        if node[0] == "BOOLEAN":
            return node[1]
        elif node[0] == "VARIABLE":
            return node[1]
        elif node[0] == "NOT":
            return f"NOT {self.print_subexpression(node[1])}"
        elif node[0] in {"AND", "OR", "IMPLIES", "EQUIVALENT"}:
            return f"({self.print_subexpression(node[1])} {node[0]} {self.print_subexpression(node[2])})"
        
def process_line(input_string, scanner, parser, evaluator, line_number=None):
    if not input_string:
        print("\nInput empty. Please enter a valid logic sentence.")
        return

    if line_number:
        print("-------------------------------------------------")
        print(f"\nProcessing line {line_number}: {input_string}")
    else:
        print("-------------------------------------------------")
        print(f"\nProcessing input: {input_string}")

    # Step 1: Tokenize the input
    try:
        tokens = scanner.scan(input_string)
    except ValueError as e:
        print("-----------------------")
        print("|    LEXICAL ERROR    |")
        print("-----------------------")
        print(e)
        return

    # Step 2: Parse the tokens
    iterator = Iterator(tokens)
    ast = parser.Sentence(iterator)
    if ast and iterator.peek() is None:
        print("----------------------------")
        print("|    Parsing successful!    |")
        print("----------------------------")
    else:
        if iterator.peek():
            parser.add_error("Unconsumed tokens remaining", iterator.peek())
        print("------------------------")
        print("|    Parsing failed!    |")
        print("------------------------")
        for error in parser.errors:
            print(f"- {error}")
        return

    # Step 3: Evaluate and generate the truth table
    variables = sorted({token[1] for token in tokens if token[0] == "VARIABLE"})
    table = evaluator.generate_truth_table(ast, variables)

    # Step 4: Output truth table
    subexpressions = evaluator.collect_subexpressions(ast)
    subexpression_headers = sorted(subexpressions, key=lambda x: str(x))  # Sort for consistent order

    # Prepare column headers
    input_header = f"{input_string}"
    table_1_headers = variables + [evaluator.print_subexpression(expr) for expr in subexpression_headers]
    table_2_header = [input_header]

    # Calculate column widths
    max_widths = {header: len(header) for header in table_1_headers + table_2_header}
    for row in table:
        for var in variables:
            max_widths[var] = max(max_widths[var], len(str(row[0][var])))
        for expr in subexpression_headers:
            expr_str = evaluator.print_subexpression(expr)
            max_widths[expr_str] = max(max_widths[expr_str], len(str(row[2].get(expr, ""))))
        max_widths[input_header] = max(max_widths[input_header], len(str(row[1])))

    # Generate table 1 (variables and subexpressions)
    header_line_1 = "\t".join(header.ljust(max_widths[header]) for header in table_1_headers)
    rows_1 = []
    for row in table:
        values = [str(row[0][var]).ljust(max_widths[var]) for var in variables]
        subresults = [str(row[2].get(expr, "")).ljust(max_widths[evaluator.print_subexpression(expr)]) for expr in subexpression_headers]
        rows_1.append("\t".join(values + subresults))

    # Generate table 2 (results)
    header_line_2 = input_header.ljust(max_widths[input_header])
    rows_2 = [str(row[1]).ljust(max_widths[input_header]) for row in table]

    # Combine tables
    combined_header = f"{header_line_1}\t{header_line_2}"
    combined_rows = [f"{row_1}\t{row_2}" for row_1, row_2 in zip(rows_1, rows_2)]

    # Print tables
    print("\n" + combined_header)
    print("\n".join(combined_rows))


# Main function to handle command-line arguments and execute the program
def main():
    print("---------------------------")
    print("|     LOGICAL EVALUATOR    |")
    print("---------------------------")
    print("Enter 1 to quit the program")

    # Initialize the Scanner, Parser, and Evaluator objects
    scanner = Scanner()
    parser = Parser()
    evaluator = Evaluator()

    # Loop to continuously prompt the user for input until they exit
    while True:
        # Prompt the user to enter input
        input_string = input("\nEnter 'sentence.pl' or your own logic sentence: ").strip()

        if input_string.endswith(".pl"):
            file_path = input_string
            try:
                with open(file_path, "r") as file:
                    lines = file.readlines()
            except FileNotFoundError:
                print("----------------")
                print("|     ERROR    |")
                print("----------------")
                print(f"File '{file_path}' not found.")
                continue

            # Process each line in the file, ignoring comments
            for line_number, line in enumerate(lines, start=1):
                # Remove comments from the line
                line = re.sub(r'#.*', '', line).strip()
                if line:  # Only process non-empty lines
                    process_line(line, scanner, parser, evaluator, line_number)

        elif input_string == "1":
            print("--------------------------")
            print("|     Exiting Program    |")
            print("--------------------------")
            print("Goodbye!")
            break
        
        # Otherwise, treat the input as a single logic sentence to process
        else:
            process_line(input_string, scanner, parser, evaluator)


if __name__ == "__main__":
    main()

