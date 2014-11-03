angular.module('HomeAutomation.Controllers')
    .controller('TemperatureHistoryController', function ($scope, $http) {
        $scope.temperatures = [
            {
                key: "Temperature",
                values: []
            }
        ];

        $scope.from = {
            date: new Date(),
            opened: false,
            open: function ($event) {
                console.log('.');
                $event.preventDefault();
                $event.stopPropagation();

                $scope.from.opened = true;
            }
        };

        $scope.to = {
            date: new Date(),
            opened: false,
            open: function ($event) {
                $event.preventDefault();
                $event.stopPropagation();

                $scope.to.opened = true;
            }
        };

        $scope.from.date.setHours(0);
        $scope.from.date.setMinutes(0);
        $scope.from.date.setSeconds(0);

        $scope.to.date.setHours(23);
        $scope.to.date.setMinutes(59);
        $scope.to.date.setSeconds(59);

        $scope.weekDays = moment.weekdays().map(function (day, idx) {
            return {
                name: day,
                selected: false,
                index: (idx - 7) % 7 + 7
            };
        });

        $scope.weekDays.push($scope.weekDays.shift());

        $scope.dateOptions = {
            formatYear: 'yy',
            startingDay: 1,
            "show-weeks": false
        };

        $scope.datePopupOptions = {
            "show-button-bar": false
        };

        $scope.xAxisTickFormatFunction = function () {
            return function (d) {               
                return d3.time.format('%y.%m.%d - %H:%M:%S')(new Date(d));
            }
        };

        $scope.toolTipContentFunction = function () {
            return function(key, x, y, e, graph) {
                return y + '&deg;C at ' + x;
            };
        };

        $scope.showForDateRange = function () {
            var from = $scope.from.date;
            var to = $scope.to.date;

            $http.get('/temperature/history?from=' + from.toISOString() + '&to=' + to.toISOString())
            .then(function (xhr) {
                $scope.temperatures[0].values = xhr.data.map(function (i) {
                    return [moment(i.timestamp).toDate().getTime(), i.temperature];
                });
            });
        };
    });