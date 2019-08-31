FROM clojure:openjdk-8-lein-2.9.1

ENV APP_HOME /app
WORKDIR $APP_HOME

COPY ./project.clj .
RUN lein deps

COPY . .

CMD ["lein", "test"]