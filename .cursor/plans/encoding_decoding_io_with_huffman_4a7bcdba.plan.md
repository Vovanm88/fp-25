---
name: Encoding Decoding IO with Huffman
overview: Реализация IO библиотеки, encoder и decoder с интеграцией кодирования Хаффмана для сжатия бинарных данных файла.
todos:
  - id: compression_encode_bytes
    content: Реализовать encode_bytes в compression.ml
    status: pending
  - id: test_compression_encode_bytes
    content: "Написать unit тесты для encode_bytes: базовые случаи, edge cases, roundtrip"
    status: pending
    dependencies:
      - compression_encode_bytes
  - id: compression_decode_bytes
    content: Реализовать decode_bytes в compression.ml
    status: pending
  - id: test_compression_decode_bytes
    content: "Написать unit тесты для decode_bytes: базовые случаи, edge cases, roundtrip с encode_bytes"
    status: pending
    dependencies:
      - compression_decode_bytes
      - test_compression_encode_bytes
  - id: compression_serialize_tree
    content: Реализовать serialize_tree в compression.ml
    status: pending
  - id: test_compression_serialize_tree
    content: "Написать unit тесты для serialize_tree: разные типы деревьев, edge cases"
    status: pending
    dependencies:
      - compression_serialize_tree
  - id: compression_deserialize_tree
    content: Реализовать deserialize_tree в compression.ml
    status: pending
  - id: test_compression_deserialize_tree
    content: "Написать unit тесты для deserialize_tree: roundtrip с serialize_tree, edge cases"
    status: pending
    dependencies:
      - compression_deserialize_tree
      - test_compression_serialize_tree
  - id: test_compression_integration
    content: "Написать интеграционные тесты для compression: полный цикл encode_bytes->serialize_tree->deserialize_tree->decode_bytes"
    status: pending
    dependencies:
      - test_compression_deserialize_tree
      - test_compression_decode_bytes
  - id: io_int32
    content: Реализовать write_int32 и read_int32 в writer.ml и reader.ml
    status: pending
  - id: test_io_int32
    content: "Написать unit тесты для int32: roundtrip, граничные значения, little-endian"
    status: pending
    dependencies:
      - io_int32
  - id: io_int16
    content: Реализовать write_int16 и read_int16 в writer.ml и reader.ml
    status: pending
  - id: test_io_int16
    content: "Написать unit тесты для int16: roundtrip, граничные значения"
    status: pending
    dependencies:
      - io_int16
  - id: io_uint8
    content: Реализовать write_uint8 и read_uint8 в writer.ml и reader.ml
    status: pending
  - id: test_io_uint8
    content: "Написать unit тесты для uint8: roundtrip, граничные значения (0, 255)"
    status: pending
    dependencies:
      - io_uint8
  - id: io_float
    content: Реализовать write_float и read_float в writer.ml и reader.ml
    status: pending
  - id: test_io_float
    content: "Написать unit тесты для float: roundtrip, точность, edge cases (0.0, отрицательные, большие числа)"
    status: pending
    dependencies:
      - io_float
  - id: io_string
    content: Реализовать write_string и read_string в writer.ml и reader.ml
    status: pending
  - id: test_io_string
    content: "Написать unit тесты для string: roundtrip, пустые строки, специальные символы"
    status: pending
    dependencies:
      - io_string
  - id: io_list
    content: Реализовать write_list и read_list в writer.ml и reader.ml (для int32, int, float)
    status: pending
  - id: test_io_list
    content: "Написать unit тесты для list: roundtrip, пустые списки, длинные списки"
    status: pending
    dependencies:
      - io_list
  - id: test_io_low_level_integration
    content: Написать интеграционные тесты для всех низкоуровневых IO функций вместе
    status: pending
    dependencies:
      - test_io_int32
      - test_io_int16
      - test_io_uint8
      - test_io_float
      - test_io_string
      - test_io_list
  - id: io_serialize_window_type
    content: Реализовать сериализацию/десериализацию window_type
    status: pending
    dependencies:
      - test_io_uint8
  - id: test_io_window_type
    content: "Написать unit тесты для window_type: все варианты, roundtrip"
    status: pending
    dependencies:
      - io_serialize_window_type
  - id: io_serialize_segment
    content: Реализовать сериализацию/десериализацию segment
    status: pending
    dependencies:
      - test_io_window_type
      - test_io_list
      - test_io_float
  - id: test_io_segment
    content: "Написать unit тесты для segment: простые и сложные случаи, с/без quantized_data, roundtrip"
    status: pending
    dependencies:
      - io_serialize_segment
  - id: io_serialize_track
    content: Реализовать сериализацию/десериализацию track
    status: pending
    dependencies:
      - test_io_segment
  - id: test_io_track
    content: "Написать unit тесты для track: с/без compression_params, несколько сегментов, roundtrip"
    status: pending
    dependencies:
      - io_serialize_track
  - id: io_serialize_audio_file
    content: Реализовать сериализацию/десериализацию audio_file
    status: pending
    dependencies:
      - test_io_track
  - id: test_io_audio_file
    content: "Написать unit тесты для audio_file: простые и сложные структуры, несколько треков, roundtrip"
    status: pending
    dependencies:
      - io_serialize_audio_file
  - id: io_serialize_with_huffman
    content: Реализовать serialize_with_huffman в writer.ml
    status: pending
    dependencies:
      - test_io_audio_file
      - test_compression_integration
  - id: test_io_serialize_huffman
    content: "Написать unit тесты для serialize_with_huffman: сравнение размеров, roundtrip"
    status: pending
    dependencies:
      - io_serialize_with_huffman
  - id: io_deserialize_with_huffman
    content: Реализовать deserialize_with_huffman в reader.ml
    status: pending
    dependencies:
      - test_io_serialize_huffman
  - id: test_io_deserialize_huffman
    content: "Написать unit тесты для deserialize_with_huffman: roundtrip с serialize_with_huffman, edge cases"
    status: pending
    dependencies:
      - io_deserialize_with_huffman
  - id: test_io_huffman_integration
    content: "Написать интеграционные тесты для IO с Хаффманом: полный цикл, сравнение с/без Хаффмана"
    status: pending
    dependencies:
      - test_io_deserialize_huffman
  - id: update_interfaces
    content: Заполнить .mli файлы для reader, writer и обновить compression.mli
    status: pending
    dependencies:
      - test_io_huffman_integration
---

# План реализации Encoding, Decoding и IO библиотеки с Хаффманом

## Архитектура

```mermaid
flowchart TD
    WAV[WAV файл] -->|read_wav| WAVData[WAV данные]
    WAVData -->|encode| AudioFile[audio_file с quantized_data]
    AudioFile -->|IO.serialize| BinaryData[Бинарные данные]
    BinaryData -->|Huffman.encode| CompressedData[Сжатые данные + дерево]
    CompressedData -->|IO.write| EncodedFile[Закодированный файл]
    
    EncodedFile -->|IO.read| CompressedData2[Сжатые данные + дерево]
    CompressedData2 -->|Huffman.decode| BinaryData2[Бинарные данные]
    BinaryData2 -->|IO.deserialize| AudioFile2[audio_file с quantized_data]
    AudioFile2 -->|decode| AudioFile3[audio_file с raw_data]
    AudioFile3 -->|write_wav| WAV2[WAV файл]
    
    style WAV fill:#e1f5ff
    style EncodedFile fill:#fff4e1
    style WAV2 fill:#e1f5ff
```

## 1. Расширение модуля Compression (`lib/codec/utilities/compression.ml`)

### Текущее состояние:

- Есть базовое кодирование Хаффмана для `char list`
- Есть функции `bools_to_bytes` и `bytes_to_bools`

### Необходимые дополнения:

- **`encode_bytes : int list -> int list * huffman_tree option`**
  - Кодирование списка байтов (int 0-255) вместо char
  - Возвращает сжатые байты и дерево Хаффмана

- **`decode_bytes : int list -> huffman_tree option -> int list`**
  - Декодирование списка байтов обратно

- **`serialize_tree : huffman_tree -> int list`**
  - Сериализация дерева Хаффмана в бинарный формат для сохранения в файл
  - Формат: рекурсивная структура (тип узла, данные, дети)

- **`deserialize_tree : int list -> huffman_tree * int list`**
  - Десериализация дерева из бинарного формата
  - Возвращает дерево и оставшиеся байты

## 2. IO библиотека (`lib/io/reader.ml`, `lib/io/writer.ml`)

### Низкоуровневые операции:

- **Reader**: чтение int32, int16, uint8, float, string, списков из бинарного потока
- **Writer**: запись int32, int16, uint8, float, string, списков в бинарный поток
- Little-endian формат (как в WAV)

### Высокоуровневая сериализация с Хаффманом:

- **`serialize_with_huffman : audio_file -> int list`**
  - Сериализует audio_file в бинарный формат
  - Применяет кодирование Хаффмана к результату
  - Возвращает: [magic, version, tree_size, tree_data, compressed_data_size, compressed_data]

- **`deserialize_with_huffman : int list -> audio_file`**
  - Читает дерево Хаффмана из данных
  - Декодирует сжатые данные
  - Десериализует в audio_file

### Формат файла с Хаффманом:

```
[Magic: 4 bytes "ACOD"]
[Version: int32]
[HasHuffman: uint8 (0/1)]
If HasHuffman:
  [TreeSize: int32] - размер сериализованного дерева в байтах
  [TreeData: TreeSize bytes] - сериализованное дерево Хаффмана
  [CompressedSize: int32] - размер сжатых данных в байтах
  [CompressedData: CompressedSize bytes] - сжатые данные
Else:
  [UncompressedSize: int32]
  [UncompressedData: UncompressedSize bytes] - несжатые данные
```

### Формат несжатых данных (внутри):

```
[NumTracks: int32]
[BitsPerSample: int32]
[SampleRate: int32]
For each track:
  [NumSegments: int32]
  [LengthSamples: int32]
  [SampleRate: int32]
  [HasCompression: uint8 (0/1)]
  [If HasCompression: OriginalMin: float, OriginalMax: float]
  For each segment:
    [WindowType: uint8]
    [NumBands: int32]
    [FrequencyBands: list of (float, float)]
    [BandRanges: list of (float, float)]
    [QuantizationLevels: list of int32]
    [HasQuantizedData: uint8 (0/1)]
    [If HasQuantizedData: QuantizedData per band]
```

## 3. Encoder и Decoder (после тестирования всех модулей)

### Будет реализовано после завершения всех тестов базовых модулей

Encoder (`lib/codec/encoder.ml`) - будет реализован позже:

- `encode_from_wav : ?use_huffman:bool -> wav_data -> string -> unit`
- `encode_from_samples : ?use_huffman:bool -> float list -> int -> int -> string -> unit`

Decoder (`lib/codec/decoder.ml`) - будет реализован позже:

- `decode_to_wav : string -> wav_data`
- `decode_to_samples : string -> float list`

## Файлы для изменения/создания:

### Compression расширение (по одной функции + тесты)

1. **[lib/codec/utilities/compression.ml](lib/codec/utilities/compression.ml)** - добавить encode_bytes → unit тесты
2. **[lib/codec/utilities/compression.ml](lib/codec/utilities/compression.ml)** - добавить decode_bytes → unit тесты
3. **[lib/codec/utilities/compression.ml](lib/codec/utilities/compression.ml)** - добавить serialize_tree → unit тесты
4. **[lib/codec/utilities/compression.ml](lib/codec/utilities/compression.ml)** - добавить deserialize_tree → unit тесты
5. **[test/codec/utilities/test_compression.ml](test/codec/utilities/test_compression.ml)** - добавить все unit тесты + интеграционные
6. **[lib/codec/utilities/compression.mli](lib/codec/utilities/compression.mli)** - обновить интерфейс

### IO низкоуровневые функции (по одной паре + тесты)

7. **[lib/io/writer.ml](lib/io/writer.ml)** + **[lib/io/reader.ml](lib/io/reader.ml)** - write_int32/read_int32 → unit тесты
8. **[lib/io/writer.ml](lib/io/writer.ml)** + **[lib/io/reader.ml](lib/io/reader.ml)** - write_int16/read_int16 → unit тесты
9. **[lib/io/writer.ml](lib/io/writer.ml)** + **[lib/io/reader.ml](lib/io/reader.ml)** - write_uint8/read_uint8 → unit тесты
10. **[lib/io/writer.ml](lib/io/writer.ml)** + **[lib/io/reader.ml](lib/io/reader.ml)** - write_float/read_float → unit тесты
11. **[lib/io/writer.ml](lib/io/writer.ml)** + **[lib/io/reader.ml](lib/io/reader.ml)** - write_string/read_string → unit тесты
12. **[lib/io/writer.ml](lib/io/writer.ml)** + **[lib/io/reader.ml](lib/io/reader.ml)** - write_list/read_list → unit тесты
13. **[test/io/test_reader_writer.ml](test/io/test_reader_writer.ml)** - все unit тесты + интеграционные
14. **[lib/io/reader.mli](lib/io/reader.mli)** + **[lib/io/writer.mli](lib/io/writer.mli)** - интерфейсы

### IO сериализация audio_file (по одной части + тесты)

15. **[lib/io/writer.ml](lib/io/writer.ml)** + **[lib/io/reader.ml](lib/io/reader.ml)** - window_type → unit тесты
16. **[lib/io/writer.ml](lib/io/writer.ml)** + **[lib/io/reader.ml](lib/io/reader.ml)** - segment → unit тесты
17. **[lib/io/writer.ml](lib/io/writer.ml)** + **[lib/io/reader.ml](lib/io/reader.ml)** - track → unit тесты
18. **[lib/io/writer.ml](lib/io/writer.ml)** + **[lib/io/reader.ml](lib/io/reader.ml)** - audio_file → unit тесты
19. **[test/io/test_serialize.ml](test/io/test_serialize.ml)** - все unit тесты для сериализации

### IO интеграция Хаффмана

20. **[lib/io/writer.ml](lib/io/writer.ml)** - serialize_with_huffman → unit тесты
21. **[lib/io/reader.ml](lib/io/reader.ml)** - deserialize_with_huffman → unit тесты
22. **[test/io/test_huffman.ml](test/io/test_huffman.ml)** - unit тесты + интеграционные тесты
23. **[lib/io/reader.mli](lib/io/reader.mli)** + **[lib/io/writer.mli](lib/io/writer.mli)** - обновить интерфейсы

### Encoder/Decoder (после всех тестов)

24. **[lib/codec/encoder.ml](lib/codec/encoder.ml)** - реализация encoder
25. **[lib/codec/encoder.mli](lib/codec/encoder.mli)** - интерфейс encoder
26. **[lib/codec/decoder.ml](lib/codec/decoder.ml)** - реализация decoder
27. **[lib/codec/decoder.mli](lib/codec/decoder.mli)** - интерфейс decoder

## Зависимости:

- Compression расширение: использует существующий код, добавляет работу с байтами
- IO библиотека зависит от: `codec` (для типа audio_file), `codec.utilities` (для Хаффмана)
- Encoder зависит от: `wav`, `codec.utilities`, `codec`, `io`
- Decoder зависит от: `codec.utilities`, `codec`, `io`

## Стратегия тестирования (TDD подход):

### Принцип: Код -> Unit тесты -> Следующий код -> Unit тесты

1. **Реализовать одну маленькую функцию/пару функций**
2. **Сразу написать unit тесты для этой функции**:

   - Базовые случаи (happy path)
   - Edge cases (пустые списки, граничные значения, None)
   - Roundtrip тесты (если применимо)

3. **Запустить тесты, убедиться что проходят**
4. **Перейти к следующей функции**

### После группы функций:

5. **Написать интеграционные тесты** для проверки взаимодействия нескольких функций вместе
6. **E2E тесты** для проверки полного цикла (если применимо)

### Структура тестов:

- Использовать Alcotest (как в существующих тестах)
- Группировать тесты по функциональности
- Использовать `test_case` с `Quick` для быстрых тестов
- Добавить `verbose` тесты для отладки при необходимости
- Каждый тест должен быть независимым и проверять одну вещь

## Важные детали:

1. **Кодирование Хаффмана**: Применяется ко всему сериализованному файлу после сериализации
2. **Сериализация дерева**: Дерево Хаффмана должно быть сериализовано компактно для сохранения в файл
3. **Обратная совместимость**: Формат должен поддерживать файлы без Хаффмана (HasHuffman = 0)
4. **Тестирование перед интеграцией**: Каждый модуль тестируется отдельно перед использованием в следующем
5. **Обработка ошибок**: Добавить исключения для невалидных данных, тестировать их
6. **Формат данных**: Little-endian для совместимости с WAV форматом