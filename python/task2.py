"""
Задача 2: Найти максимальную сумму пути в треугольнике
Реализация на Python для сравнения
"""

import time


TRIANGLE_DATA = [
    [75],
    [95, 64],
    [17, 47, 82],
    [18, 35, 87, 10],
    [20, 4, 82, 47, 65],
    [19, 1, 23, 75, 3, 34],
    [88, 2, 77, 73, 7, 63, 67],
    [99, 65, 4, 28, 6, 16, 70, 92],
    [41, 41, 26, 56, 83, 40, 80, 70, 33],
    [41, 48, 72, 33, 47, 32, 37, 16, 94, 29],
    [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14],
    [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57],
    [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48],
    [63, 66, 4, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31],
    [4, 62, 98, 27, 23, 9, 70, 98, 73, 93, 38, 53, 60, 4, 23]
]


def solve2_recursive(triangle):
    """Рекурсивная реализация"""
    def max_path_sum(row_idx, col_idx):
        if row_idx >= len(triangle):
            return 0
        
        current_val = triangle[row_idx][col_idx]
        
        if row_idx + 1 >= len(triangle):
            return current_val
        
        left_sum = max_path_sum(row_idx + 1, col_idx)
        right_sum = max_path_sum(row_idx + 1, col_idx + 1)
        
        return current_val + max(left_sum, right_sum)
    
    return max_path_sum(0, 0)


def solve2_dp(triangle):
    """Динамическое программирование (снизу вверх)"""
    rows = len(triangle)
    max_sums = [[0] * len(row) for row in triangle]
    
    # Инициализируем последнюю строку
    for i in range(len(triangle[rows - 1])):
        max_sums[rows - 1][i] = triangle[rows - 1][i]
    
    # Заполняем снизу вверх
    for row in range(rows - 2, -1, -1):
        for col in range(len(triangle[row])):
            left = max_sums[row + 1][col]
            right = max_sums[row + 1][col + 1]
            max_sums[row][col] = triangle[row][col] + max(left, right)
    
    return max_sums[0][0]


def solve2_functional(triangle):
    """Функциональная реализация с reduce"""
    from functools import reduce
    
    def process_row(prev_max, current_row):
        return [
            current_row[i] + max(prev_max[i], prev_max[i + 1])
            if i + 1 < len(prev_max)
            else current_row[i] + prev_max[i]
            for i in range(len(current_row))
        ]
    
    reversed_triangle = list(reversed(triangle))
    last_row = reversed_triangle[0]
    rest_rows = reversed_triangle[1:]
    
    result = reduce(process_row, rest_rows, last_row)
    return result[0]


def solve2_list_comprehension(triangle):
    """Реализация с list comprehension"""
    rows = len(triangle)
    max_sums = [row[:] for row in triangle]  # Копируем треугольник
    
    # Обрабатываем снизу вверх
    for row in range(rows - 2, -1, -1):
        max_sums[row] = [
            triangle[row][col] + max(max_sums[row + 1][col], max_sums[row + 1][col + 1])
            for col in range(len(triangle[row]))
        ]
    
    return max_sums[0][0]


def solve2_brute_force(triangle):
    """Полный перебор всех путей"""
    def generate_paths(row_idx, col_idx, path):
        if row_idx >= len(triangle):
            return [path]
        
        current_val = triangle[row_idx][col_idx]
        new_path = path + [current_val]
        
        if row_idx + 1 >= len(triangle):
            return [new_path]
        
        left_paths = generate_paths(row_idx + 1, col_idx, new_path)
        right_paths = generate_paths(row_idx + 1, col_idx + 1, new_path)
        
        return left_paths + right_paths
    
    all_paths = generate_paths(0, 0, [])
    path_sums = [sum(path) for path in all_paths]
    return max(path_sums)


if __name__ == "__main__":
    triangle = TRIANGLE_DATA
    
    print("=== Задача 2: Максимальная сумма пути в треугольнике ===")
    print()
    
    print("1. Рекурсивная реализация:")
    start_time = time.time()
    result1 = solve2_recursive(triangle)
    end_time = time.time()
    print(f"   Результат: {result1} (время: {end_time - start_time:.4f} сек)")
    print()
    
    print("2. Динамическое программирование:")
    start_time = time.time()
    result2 = solve2_dp(triangle)
    end_time = time.time()
    print(f"   Результат: {result2} (время: {end_time - start_time:.4f} сек)")
    print()
    
    print("3. Функциональная реализация (reduce):")
    start_time = time.time()
    result3 = solve2_functional(triangle)
    end_time = time.time()
    print(f"   Результат: {result3} (время: {end_time - start_time:.4f} сек)")
    print()
    
    print("4. Реализация с list comprehension:")
    start_time = time.time()
    result4 = solve2_list_comprehension(triangle)
    end_time = time.time()
    print(f"   Результат: {result4} (время: {end_time - start_time:.4f} сек)")
    print()
    
    print("5. Полный перебор (для сравнения):")
    start_time = time.time()
    result5 = solve2_brute_force(triangle)
    end_time = time.time()
    print(f"   Результат: {result5} (время: {end_time - start_time:.4f} сек)")
    print()

