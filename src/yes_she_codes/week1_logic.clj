(ns yes-she-codes.week1-logic
  (:require [clojure-csv.core :as csv]
            [clojure.string :as str]
            [clojure.java.io :as io])
  (:import [java.time LocalDate YearMonth Month]))

(defn novo-cliente
  ;; para aceitar lista db
  ;; this uses destructuring
  ([[nome cpf email]]
   (novo-cliente nome cpf email))

  ([nome cpf email]
   (let [cadastro {:nome  nome
                   :cpf   cpf
                   :email email}]
     cadastro)))

(comment
  (novo-cliente "MariaClara" "a-cpf" "mc@somewhere.com")
  (novo-cliente ["MariaClara" "a-cpf" "mc@somewhere.com"])
  (novo-cliente {:nome "MariaClara", :cpf "a-cpf", :email "mc@somewhere.com"}))


(defn string-to-long
  "Given a string with digits and, optionally, spaces,
  returns a long parsed from the string with the spaces
  removed."
  [string]
  (Long/parseLong (str/replace string #" " "")))

(comment
  (string-to-long "12345 6789"))

(defn novo-cartao
  ([[numero cvv validade limite cliente]]
   (novo-cartao numero cvv validade limite cliente))
  ([numero cvv validade limite cliente]
   (let [cadastro {:numero   (string-to-long numero)
                   :cvv      (string-to-long cvv)
                   :validade (YearMonth/parse validade)
                   :limite   (bigdec limite)
                   :cliente  cliente}]
     cadastro)))

(defn nova-compra
  ([data valor estabelecimento categoria cartao]
   (let [cadastro {:data            (LocalDate/parse data)
                   :valor           (bigdec valor)
                   :estabelecimento estabelecimento
                   :categoria       categoria
                   :cartao          (string-to-long cartao)}]
     cadastro))

  ([[data valor estabelecimento categoria cartao]]
   (nova-compra data valor estabelecimento categoria cartao)))

(defn abre-csv
  [path]
  (with-open [arquivo (io/reader path)]
    (csv/parse-csv (slurp arquivo))))

(defn lista-clientes
  ([]
   (let [clientes-csv (abre-csv "resources/clientes.csv")]
     (lista-clientes (rest clientes-csv))))

  ([lista-de-listas]
   (vec (map novo-cliente lista-de-listas))))


(defn lista-cartoes
  ([]
   (let [cartoes-csv (abre-csv "resources/cartoes.csv")]
     (lista-cartoes (rest cartoes-csv))))
  ([lista-de-listas]
   (vec (map novo-cartao lista-de-listas))))

(comment
  (lista-cartoes))

(defn lista-compras
  ([]
   (let [compras-csv (abre-csv "resources/compras.csv")]
     (lista-compras (rest compras-csv))))
  ([lista-de-listas]
   (vec (map nova-compra lista-de-listas))))

(defn total-gasto
  [compras]
  (let [valores (map :valor compras)]
    (reduce + valores))
  ;; could also be inlined
  #_(reduce + (map :valor compras))
  ;; "thread last":
  #_(->> compras
         (map :valor)
         (reduce +)))

(comment
  (total-gasto [{:data "2022-01-01", :valor 129.9, :estabelecimento "Outback", :categoria "Alimentação", :cartao 1234123412341234}
                {:data "2022-01-02", :valor 260.00, :estabelecimento "Dentista", :categoria "Saúde", :cartao 1234123412341234}]))

(defn mes-do-object
  [data]
  (Month/from data))

(defn compras-daquele-mes
  ;; mês int
  [mes compras]
  (let [mes-input (Month/of mes)]
    ;; (filter #(clojure.string/includes? (:data %) (str mes \-)) compras) ;sem javatime
    (filter #(= (mes-do-object (:data %)) mes-input) compras)))

(defn compras-daquele-estabelecimento
  [estabelecimento compras]
  (filter #(= (:estabelecimento %) estabelecimento) compras)
  ;; could be
  #_(filter (comp #{estabelecimento} :estabelecimento) compras)
  )

(comment
  (require '[yes-she-codes.week1_db :as db])
  (compras-daquele-estabelecimento "Alura" (lista-compras db/exemplos-compras)))

(defn total-gasto-no-mes
  [mes compras]
  (total-gasto (compras-daquele-mes mes compras)))

(defn intervalo-valor
  [min max compras]
  (let [maior? (filter #(>= (:valor %) min) compras)
        menor? (filter #(<= (:valor %) max) maior?)]
    menor?))

(defn total-categoria
  [compras]
  (let [nome-e-total (fn [[categoria info]]
                       {:categoria   categoria
                        :valor-total (total-gasto info)})]
    (->> compras
         (group-by :categoria)
         (map nome-e-total))))
