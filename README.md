# fp-25

## Lab 2
#### Вариант rb-set
#### 367390
#### Мельников Владимир Владимирович

### Задание
1. Реализовать rb-set в виде моноида (немутабельный)
2. Провести unit-тестирование
3. Провести property-based тестирование

# Лабораторная работа №2

## Титульный лист

**Вариант:** rb-set (Red-Black Tree Set)  
**Студент:** Мельников Владимир Владимирович  
**Номер зачетной книжки:** 367390  
**Дисциплина:** Функциональное программирование

---

## Требования к разработанному ПО

### Функциональные требования

1. **Базовые операции:**
   - Добавление элементов (`add`)
   - Удаление элементов (`remove`)
   - Фильтрация (`filter`)
   - Отображение (`map`)
   - Левая и правая свертки (`fold_left`, `fold_right`)

2. **Моноид:**
   - Операция объединения (`append`) с нейтральным элементом (`empty`)
   - Ассоциативность: `append (append a b) c = append a (append b c)`
   - Левая единица: `append empty a = a`
   - Правая единица: `append a empty = a`

3. **Сравнение:**
   - Эффективная функция сравнения множеств (`compare`), реализованная на уровне API

4. **Полиморфизм:**
   - Структура данных полиморфна по типу элементов
   - Использование функции сравнения для работы с произвольными типами

5. **Неизменяемость:**
   - Все операции возвращают новые структуры данных, не изменяя исходные

### Нефункциональные требования

1. **Инкапсуляция:**
   - API не должно "протекать" - все операции доступны только через публичный интерфейс
   - Внутреннее представление скрыто от пользователя

2. **Тестирование:**
   - Unit-тестирование всех операций
   - Property-based тестирование (минимум 3 свойства, включая свойства моноида)

3. **Идиоматичность:**
   - Использование идиоматичного для OCaml стиля программирования
   - Разделение интерфейса (`rb_set.mli`) и реализации (`rb_set_impl.ml`)

---

## Ключевые элементы реализации

### Структура данных

Реализация основана на красно-черном дереве (Red-Black Tree) - самобалансирующемся бинарном дереве поиска.

```ocaml
type color = Red | Black
type 'a tree = Leaf | Node of color * 'a tree * 'a * 'a tree

type 'a t = { cmp : 'a -> 'a -> int; tree : 'a Rb_set_impl.tree }
```

### Балансировка дерева

Функция `balance` обеспечивает инварианты красно-черного дерева после вставки:

```ocaml
and balance = function
  | Node (Black, Node (Red, Node (Red, a, x, b), y, c), z, d)
  | Node (Black, Node (Red, a, x, Node (Red, b, y, c)), z, d)
  | Node (Black, a, x, Node (Red, Node (Red, b, y, c), z, d))
  | Node (Black, a, x, Node (Red, b, y, Node (Red, c, z, d))) ->
      Node (Red, Node (Black, a, x, b), y, Node (Black, c, z, d))
  | t -> t
```

### Вставка элемента

```ocaml
let rec insert cmp x = function
  | Leaf -> Node (Red, Leaf, x, Leaf)
  | Node (col, l, v, r) ->
      let comp = cmp x v in
      if comp < 0 then balance (Node (col, insert cmp x l, v, r))
      else if comp > 0 then balance (Node (col, l, v, insert cmp x r))
      else Node (col, l, v, r)
```

### Удаление элемента

Использует вспомогательную функцию `remove_min` для нахождения минимального элемента:

```ocaml
let rec remove cmp x = function
  | Leaf -> Leaf
  | Node (col, l, v, r) -> (
      let comp = cmp x v in
      if comp < 0 then balance (Node (col, remove cmp x l, v, r))
      else if comp > 0 then balance (Node (col, l, v, remove cmp x r))
      else
        match remove_min r with
        | Leaf, None -> blacken l
        | Node (_, _, _, _), None -> blacken l
        | r', Some min -> balance (Node (col, l, min, r')))
```

### Объединение множеств (моноид)

```ocaml
let union cmp t1 t2 =
  match (t1, t2) with
  | Leaf, t | t, Leaf -> t
  | _ -> fold_left (fun acc x -> insert cmp x acc) t2 t1
```

### Свертки

Левая свертка:
```ocaml
let rec fold_left f acc = function
  | Leaf -> acc
  | Node (_, l, x, r) -> fold_left f (f (fold_left f acc l) x) r
```

Правая свертка:
```ocaml
let rec fold_right f = function
  | Leaf -> fun acc -> acc
  | Node (_, l, x, r) -> fun acc -> f x (fold_right f r (fold_right f l acc))
```

### Сравнение множеств

Эффективное сравнение через лексикографическое сравнение отсортированных списков элементов:

```ocaml
let compare_trees cmp t1 t2 =
  let list1 = to_list t1 in
  let list2 = to_list t2 in
  let rec compare_lists l1 l2 =
    match (l1, l2) with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | x1 :: r1, x2 :: r2 ->
        let c = cmp x1 x2 in
        if c <> 0 then c else compare_lists r1 r2
  in
  compare_lists list1 list2
```

---

## Тесты, отчет инструмента тестирования, метрики

### Структура тестирования

Проект включает четыре набора тестов:

1. **Unit-тесты** (`test_unit.ml`) - 9 тестовых случаев
2. **Golden-тесты** (`test_golden.ml`) - 4 комплексных теста
3. **Property-based тесты** (`test_pbt.ml`) - 8 свойств
4. **API leak тесты** (`test_api_leak.ml`) - 11 тестов инкапсуляции

### Unit-тестирование

Покрывает базовые операции:
- `empty` - создание пустого множества
- `add` - добавление элементов (включая дубликаты)
- `remove` - удаление элементов
- `filter` - фильтрация по предикату
- `map` - отображение элементов
- `fold_left` / `fold_right` - свертки
- `append` - объединение множеств
- `compare` - сравнение множеств

### Property-Based тестирование

Проверяются следующие свойства (по 1000 тестовых случаев каждое):

1. **Моноид - левая единица:** `append empty s = s`
2. **Моноид - правая единица:** `append s empty = s`
3. **Моноид - ассоциативность:** `append (append s1 s2) s3 = append s1 (append s2 s3)`
4. **Идемпотентность добавления:** `add x (add x s) = add x s`
5. **Коммутативность объединения:** `append s1 s2` и `append s2 s1` содержат одинаковые элементы
6. **Фильтр с истинным предикатом:** `filter (fun _ -> true) s = s`
7. **Фильтр с ложным предикатом:** `filter (fun _ -> false) s = empty`
8. **Тождественное отображение:** `map (fun x -> x) s = s`

### API Leak тестирование

Проверяет отсутствие утечек внутренней реализации:
- Все операции доступны только через публичный API
- Эффективность функции `compare`
- Невозможность прямого создания структуры данных
- Неизменяемость (immutability)
- Независимость от порядка добавления элементов

### Запуск тестов

```bash
# Все тесты
dune runtest

# Отдельные наборы
dune exec test/test_unit.exe
dune exec test/test_golden.exe
dune exec test/test_pbt.exe
dune exec test/test_api_leak.exe
```

### Метрики покрытия

- **Общее количество тестов:** 32 тестовых случая
- **Property-based тесты:** 8 свойств × 1000 случаев = 8000 проверок
- **Покрытие операций:** 100% публичного API
- **Покрытие граничных случаев:** пустые множества, дубликаты, пересечения

---

## Выводы

Сделаны.


### Области для улучшения

- Оптимизация функции `compare` для больших множеств (текущая реализация использует конвертацию в список)
- Более эффективная реализация `union` для больших деревьев
- Возможность добавления операций `intersect` и `diff`

### Заключение

Реализация успешно демонстрирует применение функционального программирования для создания эффективной и надежной структуры данных. Использование красно-черного дерева обеспечивает хорошую производительность, а comprehensive тестирование гарантирует корректность реализации.
