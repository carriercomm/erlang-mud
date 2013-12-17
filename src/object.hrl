% mnesia:create_table(object, 
%   [
%    {attributes, record_info(fields, object)}, 
%    {index, [#object.location_x, #object.location_y, #object.location_z]}
%   ]).
% mnesia:change_table_copy_type(object, node(), disc_copies).

-record(object, 
            {
             uniqueID,
             autoLoad,
             startup,
             subclass,
             location_x,
             location_y,
             location_z
            }
).
