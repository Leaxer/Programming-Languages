% place(ID)
place(adminOffice).
place(cafeteria).
place(engineeringBld).
place(lectureHallA).
place(instituteY).
place(library).
place(socialSciencesBld).
place(instituteX).

%  personel(ID, Capacity, WorkHour, CurrDelivery, CurrLocation)
personel(person1, 6, [8, 12, 16], object2, adminOffice).
personel(person2, 10, [10], none, adminOffice).
personel(person3, 15, [6, 10, 14, 18], none, library).

%  object(ID, Weight, PickPlace, DropPlace, Urgency, PersonId)
object(object1, 8, adminOffice, library, medium, none).
object(object2, 5, adminOffice, cafeteria, low, person1).
object(object3, 2, engineeringBld, instituteY, high, none).
object(object4, 11, cafeteria, lectureHallA, low, none).
object(object5, 23, socialSciencesBld, instituteY, high, none).

% edge(first_location, second_location, time)
edge(adminOffice, cafeteria, 4).
edge(adminOffice, engineeringBld, 3).
edge(adminOffice, library, 1).
edge(cafeteria, socialSciencesBld, 2).
edge(cafeteria, library, 5).
edge(socialSciencesBld, instituteX, 8).
edge(socialSciencesBld, library, 2).
edge(engineeringBld, lectureHallA, 2).
edge(engineeringBld, library, 5).
edge(lectureHallA, instituteY, 3).
edge(instituteY, library, 3).

edge(cafeteria, adminOffice, 4).
edge(engineeringBld, adminOffice, 3).
edge(library, adminOffice, 1).
edge(socialSciencesBld, cafeteria, 2).
edge(library, cafeteria, 5).
edge(instituteX, socialSciencesBld, 8).
edge(library, socialSciencesBld, 2).
edge(lectureHallA, engineeringBld, 2).
edge(library, engineeringBld, 5).
edge(instituteY, lectureHallA, 3).
edge(library, instituteY, 3).

% reverse (A->B or B->A, same road)
routes(X,Y,C) :- 
    shortest_route(X, Y, C).

% Shortest Time
shortest_route(X, Y, C) :-     
    route(X, Y, C, [X]),      
    \+ (route(X, Y, C1, [X]),  
    C1 < C).


% Find route
route(X, Y, Time, _) :-
    edge(X, Y, Time).
route(X, Y, Time, TravelledPlaces) :-
    edge(X, Z, Time1),
    \+ memberchk(Z, TravelledPlaces),
    append([Z], TravelledPlaces, NewTravelledPlaces),
    route(Z, Y, Time2, NewTravelledPlaces),
    Time is Time1 + Time2.

% person for an object in transit
in_delivery(ObjectName, PersonName) :-
    object(ObjectName, _, _, _, _, PersonName).

delivery(ObjectName, PersonName, TotalTime) :-
    (   in_delivery(ObjectName, PersonName),
        PersonName \== none ->
            write('Object is in transit. Delivery person: '), write(PersonName)
    ;   possible_personels(ObjectName, PersonName, TotalTime),
        write('Object is available for delivery. Delivery person: '), write(PersonName),
        write(', Total time: '), write(TotalTime)
    ).


% available delivery person for an object
possible_personels(ObjectName, PersonName, TotalTime) :-
    object(ObjectName, Weight, Pickup, Delivery, _, None),
    personel(PersonName, Capacity, WorkHours, None, CurrentLocation),
    Weight =< Capacity,
    (
        (CurrentLocation = Pickup ->
            PickupTime is 0
        ;   
            routes(CurrentLocation, Pickup, PickupTime)
        )
    ),
    routes(Pickup, Delivery, DeliverTime),
    TotalTime is PickupTime + DeliverTime,
    length(WorkHours, HoursLength),
    TotalTime =< HoursLength * 4.