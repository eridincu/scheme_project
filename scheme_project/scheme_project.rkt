#lang scheme

(define (TRANSPORTATION-COST farm_name) (get_tp_cost farm_name FARMS))

(define (get_tp_cost farm_name farm_list)(cond
                                              ((eqv? 0 (length farm_list)) 0)
                                              ((equal? farm_name (car(car farm_list))) (car(cdr(car farm_list))))
                                              (else (get_tp_cost farm_name (cdr farm_list)))))

(define (AVAILABLE-CROPS farm_name)(get_crops_on_farm farm_name FARMS))

(define (get_crops_on_farm farm_name farm_list)(cond
                                              ((eqv? 0 (length farm_list)) '())
                                              ((equal? farm_name (car(car farm_list))) (car(cdr(cdr(car farm_list)))))
                                              (else (get_crops_on_farm farm_name (cdr farm_list)))))
(define (INTERESTED-CROPS customer)(get_customers_crops customer CUSTOMERS))

(define (get_customers_crops customer customer_list)(cond
                                                    ((eqv? 0 (length customer_list)) '())
                                                    ((equal? customer (car(car customer_list)))(car(cdr(cdr(car customer_list)))))
                                                    (else (get_customers_crops customer (cdr customer_list)))))
(define (CONTRACT-FARMS customer)(customers_contracted_farms customer CUSTOMERS))

(define (customers_contracted_farms customer customer_list)(cond
                                                  ((eqv? 0 (length customer_list)) '())
                                                  ((equal? customer (car(car customer_list))) (car(cdr(car customer_list))))
                                                  (else (customers_contracted_farms customer (cdr customer_list)))))

(define (create_list l)
  (if (null? l)
     `()                      
     (cons (car(car l)) (create_list (cdr l)))))

(define (CONTRACT-WITH-FARM farm_name)
  (create_list (contract_finder farm_name)))

(define (contract_finder farm_name)
  (filter (lambda (x) (member farm_name (car(cdr x)))) CUSTOMERS))

(define (INTERESTED-IN-CROP crop_name)
  (create_list (crop_finder crop_name)))

(define (crop_finder crop_name)
  (filter (lambda(x) (member crop_name (car(cdr(cdr x))))) CUSTOMERS))

(define (MIN-SALE-PRICE crop_name)
  (cond
    ((cropExist? crop_name)(find_min 2147483647 (create_prices crop_name CROPS)))
    (else 0)))

(define (findCrop crop) (filter (lambda(x) (equal? (car x) crop)) CROPS))

(define (cropExist? crop)
  (cond
    ((eqv? 0 (length (findCrop crop))) #f)
    (else #t)))

(define (create_prices crop_name crop_list)
  (cond
    ((eqv? 0 (length crop_list)) '())
    ((equal? crop_name (car(car crop_list)))(cons (car(cdr(cdr(car crop_list))))(create_prices crop_name (cdr crop_list))))
    (else (create_prices crop_name (cdr crop_list)))))

(define (find_min min_price price_list)
  (cond
    ((eqv? 0 (length price_list)) min_price)
    ((<= min_price (car price_list)) (find_min min_price (cdr price_list)))
    (else (find_min (car price_list) price_list))))

(define (CROPS-BETWEEN min_price max_price)
  (remove-duplicates (create_crop_btw CROPS min_price max_price)))

(define (create_crop_btw crop_list min_price max_price)
  (cond
    ((null? crop_list)
     '())
    ((and (<= (car(cdr(cdr(car crop_list)))) max_price)  (>= (car(cdr(cdr(car crop_list)))) min_price))
     (cons (car(car crop_list))(create_crop_btw (cdr crop_list) min_price max_price)))
    (else (create_crop_btw (cdr crop_list) min_price max_price))))

(define (remove-duplicates l)
  (cond ((null? l)
         '())
        ((member (car l) (cdr l))
         (remove-duplicates (cdr l)))
        (else
         (cons (car l) (remove-duplicates (cdr l))))))

(define (farmProducts farm)
  (filter (lambda(x) (equal? (second x) farm)) CROPS))
(define (priceAtFarm farm crop)
  (let ((farm (filter (lambda(x) (equal? (car x) crop)) (farmProducts farm))))
    (cond [(equal? farm `()) 123]
          [else (+ (TRANSPORTATION-COST (second (car farm))))(car (cddr  (car farm)))])))
(define (BUY-PRICE customer crop)
  (car (sort (map (lambda (farm) (+ (TRANSPORTATION-COST farm)(priceAtFarm farm crop))) (CONTRACT-FARMS customer)) <)))

(define (TOTAL-PRICE customer)
  (calculate_total_price (INTERESTED-CROPS customer) customer 0))

(define (calculate_total_price crop_list customer sum )
  (cond
    ((null? crop_list) sum)
    (else (calculate_total_price (cdr crop_list) customer (+ sum (BUY-PRICE customer (car crop_list)))))))


         
 