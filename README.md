# Problem 036 - T3

**ACMOJ Problem ID**: 2578

## Table of Contents

- [Problem 036 - T3](#problem-036-t3)
  - [Table of Contents](#table-of-contents)
  - [Introduction](#introduction)
    - [Background](#background)
  - [Assignment Description](#assignment-description)
    - [Grade Composition](#grade-composition)
  - [Assignment Requirements](#assignment-requirements)
    - [Input Format](#input-format)
    - [Output Format](#output-format)
    - [Samples](#samples)
    - [Data Constraints](#data-constraints)
  - [Per-Testcase Resource Limits](#per-testcase-resource-limits)
  - [Test Data](#test-data)
  - [Submission Requirements](#submission-requirements)
    - [OJ Git Repository Compilation Process](#oj-git-repository-compilation-process)
    - [Git Configuration Requirements](#git-configuration-requirements)
    - [Submission Guidelines](#submission-guidelines)
    - [Evaluation Notes](#evaluation-notes)
    - [Academic Integrity](#academic-integrity)

## Introduction

### Background

Implement compressed bitset (binary compression) with bitwise operations, supporting large boolean arrays efficiently.

## Assignment Description

### Grade Composition

| Grading Component | Percentage |
| :--: | :--: |
| Pass **2578. T3** | 80% |
| Code Review | 20% |

Here are several points that need clarification:

- In the Code Review, we will **strictly examine your code style and repository organization structure, etc.**

- This assignment provides some sample data for testing, stored in the `/workspace/data/036/data_test/` directory. Note that these are not the test cases on the Online Judge. Passing all local test cases does not guarantee that you will pass the OJ tests.

- Besides the provided sample data, we also encourage you to design your own test data based on your program logic to assist debugging.

## Assignment Requirements

### Problem Description

在 C++ 中，我们通常用 `bool` 类型来存储一个要么是 $0$ 要么是 $1$ 的数。但是，`bool` 类型在内存中占用的空间是 $1$ 字节，即 $8$ bit，而我们实际只需要 $1$ bit 就可以存储一个 $0$ 或者 $1$。

因此，如果用一个 `bool` 数组来存储大量非 $0$ 即 $1$ 的数字，那么会造成空间的浪费。

这个时候，我们可以考虑用 **二进制压位** 的思想来存储这些数字。在 C++ 中，一个 `unsigned long long` 占 $8$ 个字节，有 $64$ 个 bit，我们可以用这 $64$ 个 bit 来存储 $64$ 个非 $0$ 即 $1$ 的数字。通过位运算，我们可以很方便地对其中的每一个 bit 进行操作。

二进制压位不仅能够节省空间，还能够极大地提高运算效率。例如，对于两个长度为 $n$ 的 $01$ 数组，我们想让他们进行与运算。如果用 `bool` 数组来存储，那么就需要进行 $n$ 次与运算。如果我们使用了二进制压位，那么我们只需要进行 $\lceil \frac{n}{64} \rceil$ 次与运算。

在 C++ 中，标准库提供了 `std::bitset` 类来实现二进制压位。具体可见 
<details>
<summary> std::bitset (点击查看详细信息)</summary>
<img src="https://acm.sjtu.edu.cn/OnlineJudge/oj-images/d011d8c2-e3a1-4a56-bb41-531246fe8570" style="width: 100%">
<img src="https://acm.sjtu.edu.cn/OnlineJudge/oj-images/07d96ff2-ccbc-450c-8207-e775fc4422ab" style="width: 100%">
<img src="https://acm.sjtu.edu.cn/OnlineJudge/oj-images/8a5b8862-971a-4be0-b2b6-1bd707c02083" style="width: 100%">
</details>
</br>

但是，不幸的是，`std::bitset` 的长度是固定的，我们无法在运行时改变它的长度。因此，我们需要自己实现一个二进制压位的类，其长度可以在运行时动态改变，类似 `std::vector` 。

现给出接口如下:

```C++
#include <vector>
#include <cstring>
#include <cstdint>
#include <iostream>
#include <algorithm>

struct dynamic_bitset {
    // 默认构造函数，默认长度为 0
    dynamic_bitset() = default;

    // 除非手动管理内存，否则 = default 即可
    ~dynamic_bitset() = default;

    /**
     * @brief 拷贝构造函数
     * 如果你用 std::vector 来实现，那么这个函数可以直接 = default
     * 如果你手动管理内存，则你可能需要自己实现这个函数
     */
    dynamic_bitset(const dynamic_bitset &) = default;

    /**
     * @brief 拷贝赋值运算符
     * 如果你用 std::vector 来实现，那么这个函数可以直接 = default
     * 如果你手动管理内存，则你可能需要自己实现这个函数
     */
    dynamic_bitset &operator = (const dynamic_bitset &) = default;

    // 初始化 bitset 的大小为 n ，且全为 0.
    dynamic_bitset(std::size_t n);

    /**
     * @brief 从一个字符串初始化 bitset。
     * 保证字符串合法，且最低位在最前面。
     * 例如 a =  "0010"，则有:
     * a 的第 0 位是 0
     * a 的第 1 位是 0
     * a 的第 2 位是 1
     * a 的第 3 位是 0
     */
    dynamic_bitset(const std::string &str);

    // 访问第 n 个位的值，和 vector 一样是 0-base
    bool operator [] (std::size_t n) const;
    // 把第 n 位设置为指定值 val
    dynamic_bitset &set(std::size_t n, bool val = true);
    // 在尾部插入一个位，并且长度加一
    /* 
    补充说明：这里指的是高位，
    比如0010后面push_back(1)应该变为00101
    */
    dynamic_bitset &push_back(bool val);

    // 如果不存在 1 ，则返回 true。否则返回 false
    bool none() const;
    // 如果不存在 0 ，则返回 true。否则返回 false
    bool all()  const;

    // 返回自身的长度
    std::size_t size() const;

    /**
     * 所有位运算操作均按照以下规则进行:
     * 取两者中较短的长度那个作为操作长度。
     * 换句话说，我们仅操作两者中重叠的部分，其他部分不变。
     * 在操作前后，bitset 的长度不应该发生改变。
     * 
     * 比如 a = "10101", b = "1100"
     * a |= b 之后，a 应该变成 "11101"
     * b |= a 之后，b 应该变成 "1110"
     * a &= b 之后，a 应该变成 "10001"
     * b &= a 之后，b 应该变成 "1000"
     * a ^= b 之后，a 应该变成 "01101"
     * b ^= a 之后，b 应该变成 "0110"
     */

    // 或操作，返回自身的引用。     a |= b 即 a = a | b
    dynamic_bitset &operator |= (const dynamic_bitset &);
    // 与操作，返回自身的引用。     a &= b 即 a = a & b
    dynamic_bitset &operator &= (const dynamic_bitset &);
    // 异或操作，返回自身的引用。   a ^= b 即 a = a ^ b
    dynamic_bitset &operator ^= (const dynamic_bitset &);

    /**
     * @brief 左移 n 位 。类似无符号整数的左移，最低位会补 0.
     * 例如 a = "1110"
     * a <<= 3 之后，a 应该变成 "0001110"
     * @return 返回自身的引用
     */
    dynamic_bitset &operator <<= (std::size_t n);
    /**
     * @brief 右移 n 位 。类似无符号整数的右移，最低位丢弃。
     * 例如 a = "10100"
     * a >>= 2 之后，a 应该变成 "100"
     * a >>= 9 之后，a 应该变成 "" (即长度为 0)
     * @return 返回自身的引用
     */
    dynamic_bitset &operator >>= (std::size_t n);

    // 把所有位设置为 1
    dynamic_bitset &set     ();
    // 把所有位取反
    dynamic_bitset &flip    ();
    // 把所有位设置为 0
    dynamic_bitset &reset   ();
};


```

### Input Format

See the problem description above.

### Output Format

See the problem description above.

### Samples

No sample data provided for this problem.

### Data Constraints

本题**要求**二进制压位，所有操作尽可能建立在压位的基础上实现，否则时间和空间性能可能无法达到预期。

| Subtask No. | Testcases No. | 额外考察的内容                                       | 分数   |
| ----------- | ------------- | ---------------------------------------------------- | ------ |
| $1$       | $1$         | 考察压位、所有构造函数、下标运算符重载和 `size` 函数 | $10$ |
| $2$       | $2$         | 考察 `push_back` 和 `set` 函数                   | $10$ |
| $3$       | $3$         | 考察 `none` 和 `all` 函数                        | $10$ |
| $4$       | $4$ | 考察与、或、异或运算操作 | $15$ |
| $5$       | $5$ | 考察左移和右移运算操作      | $15$ |
| $6$       | $6,7,8,9$ | 综合测试                             | $30$ |
| $7$       | $10$ | 内存泄漏检查                                         | $10$ |

特别提醒: 请不要随意修改接口! 即使你没实现某些接口，请依然在提交时保留这个接口的声明 (否则会 CE)。

允许自行添加需要的成员函数和成员变量等等 ，但需兼容原有接口 (比如可以自行添加右值相关接口)。

- **请不要使用给定头文件以外的其他任何头文件!**
-  **请不要使用给定头文件以外的其他任何头文件!**
- **请不要使用给定头文件以外的其他任何头文件!**

如有需要请事先向监考人员询问，未经允许违规使用直接 $0$ 分处理。

## Per-Testcase Resource Limits

- **Time Limit (per test case)**: 15000 ms
- **Memory Limit (per test case)**: 256 MiB
- **Disk Usage**: No disk usage is permitted.

## Test Data

The test data for this problem is located at `/workspace/data/036/data_test/`.

## Submission Requirements

### OJ Git Repository Compilation Process

For Git compilation, we will first clone the repository using a command similar to:
```bash
git clone <repo_url> . --depth 1 --recurse-submodules --shallow-submodules --no-local
```

Then we check if there is a `CMakeLists.txt` file. If it exists, we run (if not, a warning message will be displayed):
```bash
cmake .
```

Finally, we check if there is any of `GNUmakefile`/`makefile`/`Makefile` (if cmake was run previously, this will be the generated Makefile). If it exists, we run (if not, a warning message will be displayed):
```bash
make
```

After this process is complete, we will use the `code` file in the project root directory as the compilation result.

A `CMakeLists.txt` file is provided in the project. You can use or modify it as needed. The local environment has gcc-13 and g++-13 available.

### Git Configuration Requirements

**IMPORTANT**: You must create a `.gitignore` file in your project root directory to avoid OJ evaluation conflicts.

The `.gitignore` file should include at least the following entries:

```gitignore
CMakeFiles/
CMakeCache.txt
```

### Submission Guidelines

- The submitted code must be able to compile successfully through the above compilation process
- The compiled executable file name must be `code`
- The program needs to be able to read data from standard input and write results to standard output
- Please ensure the code runs correctly within the given time and space limits
- **You must use C++ or C language** to implement this assignment

### Evaluation Notes

- The evaluation system will test your program using the provided test data
- The program output must exactly match the expected output (including format)
- Exceeding time or memory limits will be judged as the corresponding error type
- Please pay attention to the overall time performance of your code and the time complexity of each part of your algorithm.

### Academic Integrity

If any violations are found during evaluation or code review (including but not limited to using unconventional methods to pass test cases), your final score may be significantly reduced or become **0 points**.
