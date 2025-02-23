-module(process_utils).

-export([get_message_count_by_name/1]).

get_message_count_by_name(Name) ->
    case whereis(Name) of
        undefined ->
            {error, process_not_found};
        Pid ->
            case process_info(Pid, message_queue_len) of
                {message_queue_len, Count} ->
                    Count;
                undefined ->
                    {error, process_not_found}
            end
    end.