import os
import sys
import subprocess


def compile_js(input_file_path, output_file_path):
    compile_res = subprocess.run(
        ["dune", "exec", "bin/main.exe", input_file_path], capture_output=True
    )

    if compile_res.returncode != 0:
        raise Exception("Compiler failed")

    output = compile_res.stdout.decode()

    with open(output_file_path, "w") as f:
        f.write(output)


def compile_c(input_file_path, output_file_path):
    run_res = subprocess.run(
        ["gcc", input_file_path, "-o", output_file_path], capture_output=True
    )

    if run_res.returncode != 0:
        raise Exception(f"GCC failed")


def run_compiled(file_path):
    run_res = subprocess.run([file_path], capture_output=True)

    if run_res.returncode != 0:
        raise Exception("Program failed")

    output = run_res.stdout.decode()
    return output


def run_node(file_path):
    run_res = subprocess.run(["node", file_path], capture_output=True)

    if run_res.returncode != 0:
        raise Exception("Node failed")

    output = run_res.stdout.decode()
    return output


def test_js(file_path):
    c_file_path = "test/test.c"
    output_file_path = "test/test.out"

    print(f"Testing {file_path}")
    compile_js(file_path, c_file_path)
    compile_c(c_file_path, output_file_path)

    output = run_compiled(output_file_path)
    expected_output = run_node(file_path)

    return (output, expected_output)


def main():
    verbose = True

    if len(sys.argv) != 2:
        print("Usage: python test.py <test-file>")
        exit(1)

    test_file = sys.argv[1]
    if not os.path.exists(test_file):
        print(f"Could not find test file: {test_file}")
        exit(1)

    (output, expected_output) = test_js(test_file)

    if output == expected_output:
        print(f"[✓] Test passed: {test_file}")
        if verbose:
            print(f"Output:\n{output}")
    else:
        print(f"[✗] Test failed: {test_file}")
        print(f"Output:\n{output}")
        print(f"Expected output:\n{expected_output}")


if __name__ == "__main__":
    main()
