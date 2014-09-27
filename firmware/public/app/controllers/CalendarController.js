﻿'use strict';

angular.module('HomeAutomation.Controllers')
    .controller('CalendarController', function ($scope, $http, $modal, $templateCache, $compile) {
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
                event.target = "water";

                $http.post('/events/new', event).success(function () {
                    $scope.calendar.fullCalendar('refetchEvents');
                });
            });
        };

        $scope.deleteEvent = function (event) {
            $http.delete('/event/' + event.id).success(function () {
                $scope.calendar.fullCalendar('refetchEvents');
            });
        };

        $scope.eventRender = function (event, element, view) {
            var dropdownMenu = $templateCache.get('event-icons.html');

            element.find(".fc-time span")
                .after(dropdownMenu);

            var scope = $scope.$new();
            scope.event = event;
            scope.x = 111;

            $compile(element)(scope);
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
                    week: 'DD.MM dddd'
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
        open: function ($event) {
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

    $scope.isRecurring = { value: true };

    $scope.weekDays = moment.weekdays().map(function (day, idx) {
        return {
            name: day,
            selected: false,
            index: idx
        };
    });

    $scope.ok = function () {
        $modalInstance.close({
            from: $scope.start_at.date,
            to: $scope.end_at.date,
            is_recurring: $scope.isRecurring.value,
            reccur_days: $scope.weekDays
                .filter(function (x) { return x.selected; })
                .map(function (x) { return x.index; })
        });
    };

    $scope.test = function() {
        alert($scope.isRecurring.value);
    };

    $scope.cancel = function () {
        $modalInstance.dismiss('cancel');
    };

    $scope.dateOptions = {
        formatYear: 'yy',
        startingDay: 1,
        "show-weeks": false
    };

    $scope.datePopupOptions = {
        "show-button-bar": false
    };
};