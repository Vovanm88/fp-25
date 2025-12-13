---
name: Audio Encoder Decoder Implementation
overview: "Реализация encoder и decoder для аудио сжатия с V-образным подходом: после каждого шага encoder сразу реализуется соответствующий шаг decoder, затем тесты. Файловый I/O и сериализация реализуются в конце, после проверки работы с высокоуровневыми структурами в памяти."
todos:
  - id: step1_encoder_compress
    content: "Encoder: реализовать измерение шумов и компрессию трека, unit тест"
    status: pending
  - id: step1_decoder_decompress
    content: "Decoder: реализовать декомпрессию трека, E2E тест 1→1'"
    status: pending
    dependencies:
      - step1_encoder_compress
  - id: step1_encoder_stereo
    content: "Encoder: реализовать LR → Mid-Side преобразование (с пропуском для моно), unit тест"
    status: pending
    dependencies:
      - step1_decoder_decompress
  - id: step1_decoder_stereo
    content: "Decoder: реализовать Mid-Side → LR преобразование (с пропуском для моно), E2E тест 1→2→2'→1'"
    status: pending
    dependencies:
      - step1_encoder_stereo
  - id: step2_encoder_silence
    content: "Encoder: реализовать замену тихих фрагментов после громких на тишину, unit тест"
    status: pending
    dependencies:
      - step1_decoder_stereo
  - id: step2_decoder_silence
    content: "Decoder: реализовать pass-through для silence, E2E тест 1→2→3→3'→2'→1'"
    status: pending
    dependencies:
      - step2_encoder_silence
  - id: step3_encoder_segmentation
    content: "Encoder: реализовать адаптивную сегментацию трека с определением attack/release и window_type, обработка коротких треков (дополнение нулями), unit тест"
    status: pending
    dependencies:
      - step2_decoder_silence
  - id: step3_encoder_windows
    content: "Encoder: реализовать создание перекрывающихся окон (50% overlap) с адаптивным windowing, unit тест"
    status: pending
    dependencies:
      - step3_encoder_segmentation
  - id: step3_decoder_windows
    content: "Decoder: реализовать склеивание окон обратно (overlap-add), unit тест"
    status: pending
    dependencies:
      - step3_encoder_windows
  - id: step3_e2e
    content: "E2E тест этапа 3: 1→2→3→4→4'→3'→2'→1' с проверкой SNR (можно без квантования)"
    status: pending
    dependencies:
      - step3_decoder_windows
  - id: step4_encoder_mdct_level1
    content: "Encoder: реализовать первый уровень MDCT (окно → frequency domain), unit тест"
    status: pending
    dependencies:
      - step3_e2e
  - id: step4_encoder_bands_split
    content: "Encoder: реализовать разделение MDCT коэффициентов на frequency bands, unit тест"
    status: pending
    dependencies:
      - step4_encoder_mdct_level1
  - id: step4_encoder_bands_imdct
    content: "Encoder: реализовать обратное преобразование bands в time domain (IMDCT), unit тест"
    status: pending
    dependencies:
      - step4_encoder_bands_split
  - id: step4_encoder_mdct_level2
    content: "Encoder: реализовать второй уровень MDCT (каждый band в time domain), unit тест"
    status: pending
    dependencies:
      - step4_encoder_bands_imdct
  - id: step4_encoder_quant_thresholds
    content: "Encoder: реализовать выбор допустимого уровня шумов квантования для каждого band, unit тест"
    status: pending
    dependencies:
      - step4_encoder_mdct_level2
  - id: step4_e2e
    content: "E2E тест этапа 4: полный цикл с двухуровневым MDCT (без квантования)"
    status: pending
    dependencies:
      - step4_encoder_quant_thresholds
  - id: step5_encoder_quantization
    content: "Encoder: реализовать квантование MDCT коэффициентов (опционально), unit тест"
    status: pending
    dependencies:
      - step4_e2e
  - id: step5_decoder_quantization
    content: "Decoder: реализовать деквантование и первый уровень IMDCT для каждого band, unit тест"
    status: pending
    dependencies:
      - step5_encoder_quantization
  - id: step5_decoder_mdct_level2
    content: "Decoder: реализовать восстановление первого уровня MDCT для каждого band, unit тест"
    status: pending
    dependencies:
      - step5_decoder_quantization
  - id: step5_decoder_bands_merge
    content: "Decoder: реализовать склеивание bands обратно в frequency domain, unit тест"
    status: pending
    dependencies:
      - step5_decoder_mdct_level2
  - id: step5_decoder_imdct_final
    content: "Decoder: реализовать финальный IMDCT (окно из frequency domain в time domain), E2E тест полного цикла"
    status: pending
    dependencies:
      - step5_decoder_bands_merge
  - id: step6_encoder_structure
    content: "Encoder: реализовать создание структуры AudioFile в памяти, unit тест"
    status: pending
    dependencies:
      - step5_decoder_imdct_final
  - id: step6_decoder_structure
    content: "Decoder: реализовать чтение структуры AudioFile из памяти, unit тест"
    status: pending
    dependencies:
      - step6_encoder_structure
  - id: step6_e2e_structure
    content: "E2E тест структуры в памяти: encoder создает → decoder читает → проверка всех полей"
    status: pending
    dependencies:
      - step6_decoder_structure
  - id: step7_serialization
    content: Реализовать сериализацию AudioFile через IO Writer, unit тест
    status: pending
    dependencies:
      - step6_e2e_structure
  - id: step7_deserialization
    content: Реализовать десериализацию AudioFile через IO Reader, unit тест
    status: pending
    dependencies:
      - step7_serialization
  - id: step7_roundtrip_serialization
    content: "КРИТИЧНО: E2E тест roundtrip сериализации (структура → bytes → структура → сравнение)"
    status: pending
    dependencies:
      - step7_deserialization
  - id: step8_encoder_file
    content: "Encoder: интегрировать запись в файл через сериализацию, E2E тест"
    status: pending
    dependencies:
      - step7_roundtrip_serialization
  - id: step8_decoder_file
    content: "Decoder: интегрировать чтение из файла через десериализацию, E2E тест"
    status: pending
    dependencies:
      - step8_encoder_file
  - id: step8_full_roundtrip
    content: "Полный E2E тест: WAV → encoder → файл → decoder → WAV с проверкой SNR (lossy-id функция)"
    status: pending
    dependencies:
      - step8_decoder_file
---

# Реализация Audio Encoder и Decoder (V-образный подход)

## Ключевые принципы

1. **V-образная разработка**: После каждого шага encoder сразу реализуется соответствующий шаг decoder
2. **Параллельное тестирование**: Можно пропускать квантование и проверять работу до квантования
3. **Поэтапное тестирование**: 

- После каждого маленького модуля → unit тест
- После большого модуля (encoder+decoder) → E2E тест (с пропуском следующих этапов)

4. **Файловый I/O в конце**: Запись/чтение в файл и сериализация реализуются после того, как encoder/decoder работают с высокоуровневыми структурами в памяти
5. **Избегать equal-сравнения**: вход и выход могут некритично различаться (из-за арифметики и дискретизации), поэтому вместо сравнения на равенство, лучше сравнивать SNR и среднюю разность

## Архитектура

Encoder принимает PCM треки из WAV и выполняет последовательность преобразований:

1. Измерение уровня шумов и компрессия трека
2. Преобразование стерео (LR → Mid-Side)
3. Замена тихих фрагментов после громких на тишину
4. Сегментация с перекрывающимися окнами (50% overlap) и определение типа окна по attack/release
5. Анализ энтропии в frequency bands и выбор уровня квантования
6. MDCT каждого band, квантование и запись в структуру

Decoder выполняет обратные операции в обратном порядке.

## Файлы для изменения/создания

- `lib/codec/encoder.ml` - основная логика encoder
- `lib/codec/encoder.mli` - интерфейс encoder
- `lib/codec/decoder.ml` - основная логика decoder  
- `lib/codec/decoder.mli` - интерфейс decoder
- `test/codec/test_encoder_decoder.ml` - тесты для encoder/decoder
- Возможно расширение `lib/codec/audiomodel.ml` для функций анализа энтропии и silence detection

## План реализации (V-образный подход)

### Этап 1: Базовые функции (compress/decompress, LR↔MS)

**1.1. Encoder: Измерение шумов и компрессия трека**

- Использовать существующую `AudioModel.compress_track`
- Добавить функцию измерения SNR между оригиналом и сжатой версией
- **Unit тест**: compress/decompress roundtrip с проверкой SNR

**1.2. Decoder: Декомпрессия трека**

- Использовать существующую `AudioModel.decompress_track`
- **E2E тест**: 1→1' (compress → decompress) с проверкой SNR

**1.3. Encoder: Стерео преобразование LR → Mid-Side**

- Использовать существующую `AudioModel.lr_to_mid_side`
- **Проверка моно-треков**: если трек моно (один канал), пропустить этот этап
- **Unit тест**: lr_to_mid_side для стерео, пропуск для моно

**1.4. Decoder: Стерео преобразование Mid-Side → LR**

- Использовать существующую `AudioModel.mid_side_to_lr`
- **Проверка моно-треков**: если трек моно, пропустить этот этап
- **E2E тест**: 1→2→2'→1' (compress → lr_to_mid_side → mid_side_to_lr → decompress) для стерео
- **E2E тест**: 1→1' (compress → decompress) для моно (без LR↔MS)

### Этап 2: Silence replacement

**2.1. Encoder: Определение тихих фрагментов после громких**

- Функция анализа RMS/энергии сигнала
- Автоматический порог для "громкого" и "тихого"
- Замена тихих фрагментов после громких на тишину
- **Unit тест**: silence detection и replacement

**2.2. Decoder: Восстановление (пока просто pass-through, т.к. silence потерян)**

- На данном этапе просто передает данные дальше
- В будущем можно добавить интерполяцию, но для lossy compression это нормально
- **E2E тест**: 1→2→3→3'→2'→1' (полный цикл до silence replacement)

### Этап 3: Сегментация с перекрывающимися окнами

**3.1. Encoder: Сегментация трека на кусочки**

- Адаптивная сегментация по времени (на основе attack/release характеристик)
- Определение attack/release для каждого сегмента
- Использование FSM `AudioModel.determine_window_type` для определения типа окна
- **Обработка коротких треков**: если трек короче минимального размера окна, дополнить нулями до минимального размера и записать информацию об исходной длине в метаданные
- **Unit тест**: сегментация и определение window_type
- **Unit тест**: обработка коротких треков (дополнение нулями)

**3.2. Encoder: Создание перекрывающихся окон (50% overlap)**

- Разбиение сегментов на окна с 50% overlap
- Применение windowing функции (адаптивный выбор: Hamming для Short, Hanning для Long, etc.)
- Размер окна зависит от типа (Short, Long, VeryLong, etc.)
- **Unit тест**: создание окон с overlap и windowing

**3.3. Decoder: Склеивание окон обратно**

- Overlap-add метод для склеивания перекрывающихся окон
- Применение обратного windowing
- **Unit тест**: overlap-add склеивание

**3.4. E2E тест этапа 3**

- **E2E тест**: 1→2→3→4→4'→3'→2'→1' (полный цикл до сегментации) с проверкой SNR
- Можно пропустить квантование и проверить что сегментация/склеивание работают корректно

### Этап 4: Frequency bands и энтропия

**4.1. Encoder: Первый уровень MDCT (окно → frequency domain)**

- Применение `Dsp.mdct_transform` к каждому окну
- Получение MDCT коэффициентов (frequency domain представление окна)
- **Unit тест**: MDCT roundtrip для окна (MDCT → IMDCT с overlap-add)

**4.2. Encoder: Разделение на frequency bands в frequency domain**

- Разделение MDCT коэффициентов на frequency bands (по частотам)
- Настраиваемое количество bands (по умолчанию использовать mel-spec подобное распределение)
- Каждый band содержит подмножество MDCT коэффициентов
- Расчет энтропии для каждого band в frequency domain
- Определение важности band для человеческого слуха (веса для разных частот)
- **Unit тест**: разделение MDCT коэффициентов на bands

**4.3. Encoder: Обратное преобразование bands в time domain**

- Для каждого band: применение `Dsp.imdct_transform` к MDCT коэффициентам band
- Получение time domain представления каждого band
- **Unit тест**: MDCT → bands → IMDCT для каждого band

**4.4. Encoder: Второй уровень MDCT (каждый band в time domain)**

- Применение `Dsp.mdct_transform` к каждому band в time domain
- Получение финальных MDCT коэффициентов для каждого band
- **Unit тест**: MDCT roundtrip для band (MDCT → IMDCT)

**4.5. Encoder: Выбор допустимого уровня шумов квантования**

- На основе энтропии и важности band
- Использование `AudioModel.find_optimal_quantization` с адаптивными SNR threshold
- **Unit тест**: выбор quantization levels

**4.6. Decoder: Пока pass-through (полный цикл будет после квантования)**

- **E2E тест**: 1→2→3→4→5→6→7→7'→6'→5'→4'→3'→2'→1' (до квантования, с двухуровневым MDCT)

### Этап 5: Квантование и обратное преобразование

**5.1. Encoder: Квантование MDCT коэффициентов (опционально для тестирования)**

- Подбор числа уровней квантования на основе выбранных threshold
- Квантование MDCT коэффициентов каждого band (полученных на этапе 4.4)
- Использование `Quantization.quantize_with_range` и `Quantization.encode_quantized_indices`
- **Unit тест**: квантование/деквантование MDCT коэффициентов

**5.2. Decoder: Деквантование и первый уровень IMDCT**

- Деквантование MDCT коэффициентов каждого band
- Применение `Dsp.imdct_transform` к каждому band (обратное преобразование второго уровня MDCT)
- Получение time domain представления каждого band
- **Unit тест**: деквантование → IMDCT roundtrip для band

**5.3. Decoder: Восстановление первого уровня MDCT для каждого band**

- Применение `Dsp.mdct_transform` к каждому band в time domain (после IMDCT на этапе 5.2)
- Получение MDCT коэффициентов первого уровня для каждого band
- Это обратное преобразование второго уровня MDCT encoder'а

**5.4. Decoder: Склеивание bands обратно в frequency domain**

- Объединение MDCT коэффициентов всех bands обратно в полный набор MDCT коэффициентов окна
- Восстановление полного frequency domain представления окна

**5.5. Decoder: Финальный IMDCT (окно из frequency domain в time domain)**

- Применение `Dsp.imdct_transform` к полным MDCT коэффициентам окна
- Получение time domain представления окна
- **Unit тест**: полный цикл MDCT → bands → IMDCT → MDCT → IMDCT

**5.6. E2E тест полного цикла**

- **E2E тест**: 1→2→3→4→5→6→7→8→9→9'→8'→7'→6'→5'→4'→3'→2'→1' (полный цикл с двухуровневым MDCT, можно с/без квантования)

### Этап 6: Структура AudioFile (в памяти)

**6.1. Encoder: Создание структуры AudioFile**

- Создание `AudioModel.audio_file` со всеми треками и сегментами
- Заполнение всех полей: segments, bands, quantized_data, метаданные
- **Сохранение информации о коротких треках**: запись исходной длины трека (до дополнения нулями) в метаданные
- **Сохранение информации о моно-треках**: запись информации о количестве каналов
- **Unit тест**: создание и проверка структуры

**6.2. Decoder: Чтение структуры AudioFile (из памяти)**

- Извлечение данных из структуры
- Восстановление всех полей
- **Восстановление коротких треков**: обрезание трека до исходной длины (удаление дополненных нулей) на основе метаданных
- **Восстановление моно-треков**: обработка моно-треков без LR↔MS преобразования
- **Unit тест**: чтение структуры

**6.3. E2E тест структуры (в памяти)**

- **E2E тест**: encoder создает структуру → decoder читает структуру → проверка всех полей
- Структурное сравнение: все поля должны совпадать
- **Это критично: структура должна работать в памяти ДО сериализации**

### Этап 7: Сериализация/десериализация (файловый I/O)

**7.1. Сериализация AudioFile через IO Writer**

- Реализация записи всех полей структуры `AudioModel.audio_file`
- Сериализация tracks, segments, bands, quantized_data и всех метаданных
- **Unit тест**: сериализация структуры в bytes

**7.2. Десериализация AudioFile через IO Reader**

- Реализация чтения всех полей структуры
- Восстановление полной структуры из байтов
- **Unit тест**: десериализация bytes в структуру

**7.3. Тест roundtrip сериализации (КРИТИЧНО)**

- **E2E тест**: структура → сериализация → десериализация → сравнение структур
- Проверить, что все поля совпадают (структурное сравнение)
- **Этот тест должен проходить ДО использования в полном encoder/decoder**

### Этап 8: Интеграция сериализации в encoder/decoder

**8.1. Encoder: Запись в файл**

- Использование сериализации для записи структуры в файл
- **E2E тест**: encoder → файл (проверка что файл создан и корректен)

**8.2. Decoder: Чтение из файла**

- Использование десериализации для чтения структуры из файла
- **E2E тест**: файл → decoder (проверка что структура восстановлена)

**8.3. Полный roundtrip тест**

- **E2E тест**: WAV → encoder → файл → decoder → WAV
- Проверка SNR (lossy compression, но должно быть приемлемое качество)
- Убедиться что функция lossy-id работает (SNR > 30dB для приемлемого качества)

## Детали реализации

### Перекрывающиеся окна

- 50% overlap между соседними окнами
- Размер окна зависит от window_type:
- Short: 512 samples (≈11.6ms @ 44.1kHz)
- Long: 2048 samples (≈46.4ms)
- VeryLong: 4096 samples (≈92.9ms)
- EndFocused/StartFocused: адаптивно

### Windowing функции (адаптивный выбор)

- Short → Hamming
- Long → Hanning  
- VeryLong → Hanning
- EndFocused/StartFocused → Hamming

### Mel-spec подобные bands

- Больше bands для важных частот (1-4kHz)
- Использовать логарифмическую шкалу для частот
- По умолчанию: ~20-30 bands вместо 10

### SNR проверки в тестах

- Для lossy compression ожидать SNR > 30dB (приемлемое качество)
- Для lossless шагов (LR↔MS, compress/decompress) ожидать очень высокий SNR или точное совпадение

### Пропуск квантования для тестирования

- Можно реализовать режим "без квантования" для проверки промежуточных этапов
- Это позволит изолировать проблемы сегментации/MDCT от проблем квантования

### Двухуровневая MDCT структура

- **Первый уровень**: MDCT окна → frequency domain (полный набор MDCT коэффициентов)
- **Разделение на bands**: разделение MDCT коэффициентов на frequency bands в frequency domain
- **Обратное преобразование bands**: IMDCT каждого band → time domain
- **Второй уровень**: MDCT каждого band в time domain → финальные MDCT коэффициенты для квантования
- **Decoder**: обратный процесс: деквантование → IMDCT (второй уровень) → MDCT (первый уровень) → IMDCT (финальный)

### Обработка граничных случаев

- **Моно-треки**: пропуск этапа LR↔MS преобразования, сохранение информации о количестве каналов
- **Короткие треки**: дополнение нулями до минимального размера окна, сохранение исходной длины в метаданных
- **Восстановление**: обрезание до исходной длины на основе метаданных

## Структура тестов

Тесты организованы по этапам:

1. **Unit тесты** для каждой маленькой функции (encoder и decoder отдельно)
2. **E2E тесты** для комбинаций шагов после каждого большого модуля (encoder+decoder)
3. **Тест структуры в памяти** перед сериализацией
4. **Тест roundtrip сериализации** перед использованием в encoder/decoder
5. **Полный roundtrip тест** в конце (WAV → файл → WAV)

Каждый этап тестируется перед переходом к следующему.