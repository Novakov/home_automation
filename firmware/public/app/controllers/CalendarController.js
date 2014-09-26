'use strict';

angular.module('HomeAutomation.Controllers')
    .controller('CalendarController', function ($scope, $http, $modal) {
        $scope.eventSources = {
            url: '/events',
            eventDataTransform: function (e) {
                return {
                    id: e.series_id,
                    start: e.from,
                    end: e.to,
                    icons: ['']
                }
            }
        };

        $scope.addNewEvent = function (start, end) {
            var modalInstance = $modal.open({
                templateUrl: 'app/partials/new_event.html',
                controller: NewEventController,
                resolve: {
                    start_at: function () { return start.local().toDate(); },
                    end_at: function () { return end.local().toDate(); }
                }
            });

            modalInstance.result.then(function (event) {
                var data = {
                    from: event.start,
                    to: event.end,
                    target: "water"
                };                

                $http.post('/events/new', data).success(function() {
                    $scope.calendar.fullCalendar('refetchEvents');
                });
            });
        };

        $scope.eventRender = function (event, element, view) {
            var html = '<span class="icons">';

            if (event.icons != undefined) {
                for (var idx in event.icons) {
                    html += '<span class="glyphicon glyphicon-' + event.icons[idx] + '"></span>';
                }
            }

            html += '</span>';
            
            element.find(".fc-time span").after($(html));
        };

        $scope.uiConfig = {
            calendar: {
                defaultView: 'agendaWeek',
                height: 750,
                firstDay: 1,
                allDaySlot: false,
                editable: true,
                selectable: true,
                selectHelper: true,
                columnFormat: {
                    week: 'd.MM dddd'
                },
                axisFormat: { 'agenda': 'H:mm' },
                timeFormat: 'H:mm',

                startParam: 'from',
                endParam: 'to',

                select: $scope.addNewEvent,
                eventRender: $scope.eventRender
            }
        };
    });

var NewEventController = function ($scope, $modalInstance, start_at, end_at) {
    $scope.start_at = {
        date: start_at,
        opened: false,
        open: function($event) {
            $event.preventDefault();
            $event.stopPropagation();

            $scope.start_at.opened = true;
        }
    };

    $scope.end_at = {
        date: end_at,
        opened: false,
        open: function ($event) {
            $event.preventDefault();
            $event.stopPropagation();

            $scope.end_at.opened = true;
        }
    };
   
    $scope.ok = function() {
        $modalInstance.close({
            start: $scope.start_at.date,
            end: $scope.end_at.date
        });
    };
    $scope.cancel = function() {
        $modalInstance.dismiss('cancel');
    };
   
    $scope.dateOptions = {
        formatYear: 'yy',
        startingDay: 1,
        "show-weeks": false
    };

    $scope.datePopupOptions = {
        "show-button-bar":false
    };
};