"""
Задача 1: Найти первое треугольное число с более чем 500 делителями
Реализация на Python для сравнения
"""

import time
import math


def triangle_number(n):
    """Вычисление n-го треугольного числа"""
    return n * (n + 1) // 2


def count_divisors(n):
    """Подсчёт количества делителей числа"""
    count = 0
    sqrt_n = int(math.sqrt(n))
    for i in range(1, sqrt_n + 1):
        if n % i == 0:
            if i * i == n:
                count += 1
            else:
                count += 2
    return count


def solve1_imperative(min_divisors):
    """Императивная реализация с циклом while"""
    n = 1
    while True:
        tri = triangle_number(n)
        divisors = count_divisors(tri)
        if divisors > min_divisors:
            return tri
        n += 1


def solve1_for_loop(min_divisors):
    """Реализация с циклом for"""
    for n in range(1, 20000):
        tri = triangle_number(n)
        divisors = count_divisors(tri)
        if divisors > min_divisors:
            return tri
    return None


def solve1_functional(min_divisors):
    """Функциональная реализация с генераторами"""
    def triangle_numbers():
        n = 1
        while True:
            yield triangle_number(n)
            n += 1
    
    def filter_divisors(triangles):
        for tri in triangles:
            if count_divisors(tri) > min_divisors:
                yield tri
    
    return next(filter_divisors(triangle_numbers()))


def solve1_list_comprehension(min_divisors):
    """Реализация с list comprehension"""
    limit = 20000
    triangles = [triangle_number(n) for n in range(1, limit + 1)]
    with_divisors = [(tri, count_divisors(tri)) for tri in triangles]
    filtered = [tri for tri, divs in with_divisors if divs > min_divisors]
    return filtered[0] if filtered else None


if __name__ == "__main__":
    min_divisors = 500
    
    print("=== Задача 1: Треугольные числа с более чем 500 делителями ===")
    print()
    
    print("1. Императивная реализация (while):")
    start_time = time.time()
    result1 = solve1_imperative(min_divisors)
    end_time = time.time()
    print(f"   Результат: {result1} (время: {end_time - start_time:.4f} сек)")
    print()
    
    print("2. Реализация с for-циклом:")
    start_time = time.time()
    result2 = solve1_for_loop(min_divisors)
    end_time = time.time()
    print(f"   Результат: {result2} (время: {end_time - start_time:.4f} сек)")
    print()
    
    print("3. Функциональная реализация (генераторы):")
    start_time = time.time()
    result3 = solve1_functional(min_divisors)
    end_time = time.time()
    print(f"   Результат: {result3} (время: {end_time - start_time:.4f} сек)")
    print()
    
    print("4. Реализация с list comprehension:")
    start_time = time.time()
    result4 = solve1_list_comprehension(min_divisors)
    end_time = time.time()
    print(f"   Результат: {result4} (время: {end_time - start_time:.4f} сек)")
    print()

