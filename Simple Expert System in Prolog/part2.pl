% Classes Iris Versicolour, Iris Setosa, , or Iris Virginica
%sepal length
%sepal width
%petal length
%petal width

   classify(SepalLength, SepalWidth, PetalLength, PetalWidth) :-
    (
        PetalWidth >= 0.1, PetalWidth =< 0.6
            ->   (
                    SepalLength >= 4.3, SepalLength =< 5.8, SepalWidth >= 2.3, SepalWidth =< 4.4, PetalLength >= 1.0, PetalLength =< 1.9
                        ->   write('Iris-setosa')
                )
        ; (
             PetalWidth >= 1.0, PetalWidth < 1.4
            ->   (
                        SepalLength >= 4.9, SepalLength =< 7.0, SepalWidth >= 2.0, SepalWidth =< 3.4, PetalLength >= 3.0, PetalLength =< 5.1
                            ->   write('Iris-versicolour')
                        )
                ;
                (
                PetalWidth >= 1.4, PetalWidth =< 1.8
                ->   (
                            SepalLength >= 4.9, SepalLength =< 7.0
                            ->   (
                                    SepalWidth < 2.2
                                    ->   (
                                               SepalWidth >= 2.0 -> (
                                                 PetalLength >= 3.0, PetalLength =< 5.1
                                                    ->   write('Iris-versicolour')
                                               )
                                            )
                                        ;
                                    SepalWidth =< 3.8
                                        ->   (
                                            SepalWidth > 3.4 -> (
                                                  PetalLength >= 4.5, PetalLength =< 6.9
                                                    ->   write('Iris-virginica')
                                               )
                                               ;
                                               (
                                                PetalLength >= 3.0, PetalLength < 4.5
                                                ->   write('Iris-versicolour')
                                                    ;
                                                    (
                                                        PetalLength > 5.1, PetalLength =< 6.9 -> write('Iris-virginica')
                                                        )
                                                )
                                            )
                                    )
                                ;
                                (
                                SepalLength > 7.0, SepalLength =< 7.9
                                ->   (
                                        SepalWidth >= 2.2, SepalWidth =< 3.8, PetalLength >= 4.5, PetalLength =< 6.9
                                        ->   write('Iris-virginica')
                                    )
                                )
                     )
                    ;
                PetalWidth > 1.8, PetalWidth =< 2.5
                    ->   (
                        SepalLength >= 4.9, SepalLength =< 7.9, SepalWidth >= 2.2, SepalWidth =< 3.8, PetalLength >= 4.5, PetalLength =< 6.9
                            ->   write('Iris-virginica')
                        )
                )
          )
    ).