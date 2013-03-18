<actionLogFilterForm method="GET">
  <dfErrorList ref=""/>

  <dfLabel ref="user">User</dfLabel>
  <dfInputSelect ref="user" />
  <dfErrorList ref="user"/>
  <br>

  <dfLabel ref="entity">Entity</dfLabel>
  <dfInputSelect ref="entity"/>
  <dfErrorList ref="entity"/>
  <br>

  <dfLabel ref="entity-id">Entity ID</dfLabel>
  <dfInputText ref="entity-id"/>
  <dfErrorList ref="entity-id"/>
  <br>

  <dfLabel ref="action">Action</dfLabel>
  <dfInputSelect ref="action" />
  <dfErrorList ref="action"/>
  <br>

  <dfInputSubmit class="btn btn-success" value="Refresh"/>
</actionLogFilterForm>
