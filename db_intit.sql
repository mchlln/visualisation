CREATE TABLE equipment_access (
    id_src TEXT,
    x REAL,
    y REAL,
    type TEXT,
    dist REAL,
    dist_eucl REAL,
    duration REAL,
    depcom TEXT,
    iris TEXT,
    pop INT,
    domain TEXT,
    depcom_eq TEXT,
    dep TEXT,
    reg TEXT
);

COPY product_example FROM 'data/donnees-2024-reg32.parquet';

SELECT * FROM product_example LIMIT 10;